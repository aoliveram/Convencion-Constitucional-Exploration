# --- Load Required Libraries ---
library(readr)
library(pscl)       # For ideal() function
library(wnominate)  # For rollcall() object creation
library(data.table) # For efficient data handling, reshaping, and writing
library(dplyr)      # For optional data manipulation steps if needed
library(foreach)

# --- Helper Functions ---

# Function to clean legislator names (remove accents, etc.)
normalizar_nombres <- function(texto) {
  texto_limpio <- iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT")
  return(texto_limpio)
}

# Function to validate uniqueness of legislator names in rollcall object
validar_unicidad <- function(rc_object) {
  stopifnot(length(unique(rownames(rc_object$votes))) == nrow(rc_object$votes))
}

# --- To Run ideal(), Extract MCMC SAMPLES and MEAN Positions ---

run_ideal_estimation_combined <- function(votaciones, votantes, dataset_name,
                                          dims = 2, # Request 2 dimensions
                                          mcmc_iter = 12000,
                                          mcmc_burnin = 2010,
                                          mcmc_thin = 10) {
  
  cat("\nProcessing dataset:", dataset_name, "for MCMC samples and mean positions\n")
  
  # Create rollcall object
  rc <- rollcall(votaciones,
                 yea = 1, nay = 0, missing = NA,
                 legis.names = votantes,
                 desc = paste("IDEAL Estimation (Samples & Means) for", dataset_name))
  
  # Validate legislator names are unique
  validar_unicidad(rc)
  
  cat("Running ideal() estimation (d=", dims, ") for", dataset_name, "...\n")
  start_time <- Sys.time()
  
  ideal_result <- NULL # Initialize result variable
  estimation_successful <- FALSE
  
  tryCatch({
    # Run the ideal() function (pscl v1.5.9 compatible)
    ideal_result <- ideal(object = rc,
                          d = dims,             # Number of dimensions to estimate
                          maxiter = mcmc_iter,  # MCMC Iterations
                          burnin = mcmc_burnin, # Burn-in
                          thin = mcmc_thin,     # Thinning
                          impute = TRUE,        # Impute Missing Votes
                          normalize = TRUE,     # Normalize Legislators (mean 0, sd 1)
                          store.item = FALSE,   # Don't need item parameters
                          verbose = TRUE        # Print Progress
    )
    estimation_successful <- TRUE
  }, error = function(e) {
    cat("ERROR running ideal() for dataset:", dataset_name, "\n")
    cat("Error message:", conditionMessage(e), "\n")
    ideal_result <<- NULL
  })
  
  end_time <- Sys.time()
  execution_time <- end_time - start_time
  cat("Estimation for", dataset_name, "took:", execution_time, "\n")
  
  # --- Initialize outputs ---
  results_dt_samples <- data.table()
  results_dt_means <- data.table()
  
  # --- Extract and Reshape MCMC Samples if Estimation Succeeded ---
  if (estimation_successful && !is.null(ideal_result)) {
    
    # Check for MCMC samples in ideal_result$x
    if ("x" %in% names(ideal_result) && !is.null(ideal_result$x) && is.array(ideal_result$x)) {
      
      x_samples <- ideal_result$x
      sample_dims <- dim(x_samples) # Should be [n_samples, n_legis, n_dims_estimated]
      n_samples <- sample_dims[1]
      n_legis <- sample_dims[2]
      n_dims_estimated <- sample_dims[3]
      
      if(length(sample_dims) != 3 || n_dims_estimated < 1) {
        cat("WARNING: ideal_result$x has unexpected dimensions:", sample_dims, "for", dataset_name, "\n")
        # Return empty list if samples are bad
        return(list(samples = results_dt_samples, means = results_dt_means))
      }
      
      # Get legislator names
      legislator_names <- dimnames(x_samples)[[2]]
      if(is.null(legislator_names) || length(legislator_names) != n_legis){
        cat("WARNING: Legislator names not found or mismatch in ideal_result$x for", dataset_name, ". Using input 'votantes'.\n")
        if(length(votantes) == n_legis){
          legislator_names <- votantes
        } else {
          cat("ERROR: Dimension mismatch between ideal_result$x and input 'votantes' for", dataset_name, "\n")
          return(list(samples = results_dt_samples, means = results_dt_means))
        }
      }
      
      # --- 1. Create the Full Samples Output (results_dt_samples) ---
      iter_vec <- rep(1:n_samples, each = n_legis)
      legis_vec <- rep(legislator_names, times = n_samples)
      coord1D_vec <- as.vector(t(x_samples[ , , 1, drop = TRUE])) # Dim 1
      
      results_dt_samples <- data.table(
        iteracion = iter_vec,
        legislador = legis_vec,
        coord1D = coord1D_vec
      )
      
      if (n_dims_estimated >= 2) {
        coord2D_vec <- as.vector(t(x_samples[ , , 2, drop = TRUE])) # Dim 2
        results_dt_samples[, coord2D := coord2D_vec]
      } else {
        results_dt_samples[, coord2D := NA_real_]
      }
      # Add more dimensions if needed (coord3D etc.)
      
      cat("Successfully extracted and reshaped MCMC samples for", dataset_name, "\n")
      
      
      # --- 2. Create the Mean Positions Output (results_dt_means) ---
      # Calculate posterior mean for each legislator across samples
      posterior_means_d1 <- colMeans(x_samples[ , , 1, drop = TRUE], na.rm = TRUE)
      
      results_dt_means <- data.table(
        nombre_votante = legislator_names,
        posicion_ideologica = posterior_means_d1 # Mean of Dim 1
      )
      
      # Add mean for Dim 2 if estimated
      if (n_dims_estimated >= 2) {
        posterior_means_d2 <- colMeans(x_samples[ , , 2, drop = TRUE], na.rm = TRUE)
        results_dt_means[, ideal_point_mean2D := posterior_means_d2] # Add Dim 2 mean
      }
      
      # Order by the first dimension ('posicion_ideologica')
      results_dt_means <- results_dt_means[order(posicion_ideologica)]
      
      # Add ranking column if desired (optional, similar to WN example)
      # results_dt_means[, posicion_izq_der := 1:.N]
      
      cat("Successfully calculated mean positions for", dataset_name, "\n")
      
      
    } else {
      cat("WARNING: MCMC samples ('x') not found or not an array in ideal_result object for", dataset_name, "\n")
      # Return empty list if samples are missing
      return(list(samples = results_dt_samples, means = results_dt_means))
    }
    
  } else {
    cat("Returning empty results for dataset:", dataset_name, "\n")
    # Return empty list if estimation failed
    return(list(samples = results_dt_samples, means = results_dt_means))
  }
  
  # --- START MODIFICATION: Sign Flipping Logic ---
  # Define the target legislator (using the already normalized name format)
  target_legislator <- "Marinovic Vial, Teresa" # Assumes normalizar_nombres doesn't change this specific name
  
  # Check if results were generated and if the target legislator exists in the means table
  if (nrow(results_dt_means) > 0 && target_legislator %in% results_dt_means$nombre_votante) {
    
    # Get the mean position of the target legislator in the first dimension
    target_pos_d1 <- results_dt_means[nombre_votante == target_legislator, posicion_ideologica]
    
    # Check if the position is uniquely found, not NA, and negative
    if (length(target_pos_d1) == 1 && !is.na(target_pos_d1) && target_pos_d1 < 0) {
      cat("INFO: Flipping signs for Dim 1 based on", target_legislator, "'s position in dataset:", dataset_name, "\n")
      
      # Flip signs in the means data table (Dim 1: posicion_ideologica)
      results_dt_means[, posicion_ideologica := -posicion_ideologica]
      
      # Re-order the means table by the flipped first dimension
      # The original code already orders it, so we re-apply after flipping
      results_dt_means <- results_dt_means[order(posicion_ideologica)]
      
      # Flip signs in the samples data table (Dim 1: coord1D)
      if (nrow(results_dt_samples) > 0) {
        results_dt_samples[, coord1D := -coord1D]
      }
      
    } else if (length(target_pos_d1) != 1 || is.na(target_pos_d1)) {
      cat("WARNING: Could not find unique, non-NA position for", target_legislator, "in dataset:", dataset_name, "- Sign flipping check skipped.\n")
    }
  } else {
    # This handles cases where the means table is empty or the legislator isn't found
    if (nrow(results_dt_means) > 0) { # Only warn if means were generated but legislator missing
      cat("WARNING:", target_legislator, "not found in mean results for dataset:", dataset_name, "- Sign flipping check skipped.\n")
    }
  }
  # --- END MODIFICATION ---
  
  # --- Return both data tables in a list ---
  return(list(samples = results_dt_samples, means = results_dt_means))
}


#------------------------------------------------------------------------------
# Estimación de Ordenamiento Político usando pscl::ideal (Samples & Means)
#------------------------------------------------------------------------------

# Define the datasets to process
# Updated output suffixes to reflect the two types of files
datasets_info <- list(
  list(name = "01_15", file = "scripts - files/votaciones_01_15.csv", output_suffix = "01-15_ideal"),
  list(name = "16_21", file = "scripts - files/votaciones_16_21.csv", output_suffix = "16-21_ideal"),
  list(name = "22_37", file = "scripts - files/votaciones_22_37.csv", output_suffix = "22-37_ideal"),
  list(name = "38_46", file = "scripts - files/votaciones_38_46.csv", output_suffix = "38-46_ideal"),
  list(name = "47_55", file = "scripts - files/votaciones_47_55.csv", output_suffix = "47-55_ideal"),
  list(name = "56_75", file = "scripts - files/votaciones_56_75.csv", output_suffix = "56-75_ideal"),
  list(name = "76_99", file = "scripts - files/votaciones_76_99.csv", output_suffix = "76-99_ideal"),
  list(name = "100_106", file = "scripts - files/votaciones_100_106.csv", output_suffix = "100-106_ideal"),
  list(name = "107_109", file = "scripts - files/votaciones_107_109.csv", output_suffix = "107-109_ideal")
)

# --- NON - PARALLEL RUNNIG ----------------------------------
# ---
# --- 

all_ideal_sample_results <- list() # Store full sample results
all_ideal_mean_results <- list()   # Store mean results

# Reminder: This loop runs (almost) sequentially. Run PARALLEL
start_time <- Sys.time()
for (ds_info in datasets_info) {
  cat("\n=====================================================\n")
  cat("Processing dataset:", ds_info$name, "\n")
  cat("=====================================================\n")
  
  ideal_output_samples <- NULL # Initialize
  ideal_output_means <- NULL   # Initialize
  
  tryCatch({ # Wrap data loading and estimation
    # --- Load and Preprocess Data ---
    votaciones_df <- read.csv(ds_info$file)
    
    # Preprocessing checks...
    if (ncol(votaciones_df) <= 1 || !(is.character(votaciones_df[[1]]) || is.factor(votaciones_df[[1]]))) {
      cat("WARNING: Data format issue or missing columns for", ds_info$name, "- Skipping.\n")
      next
    }
    if(is.factor(votaciones_df[[1]])) votaciones_df[[1]] <- as.character(votaciones_df[[1]])
    votaciones_df <- votaciones_df[votaciones_df[[1]] != "Rojas Vade, Rodrigo", , drop = FALSE]
    if (nrow(votaciones_df) == 0) {
      cat("WARNING: No rows left after filtering for", ds_info$name, "- Skipping.\n")
      next
    }
    votantes <- normalizar_nombres(as.vector(votaciones_df[[1]]))
    votaciones_data <- votaciones_df[,-1, drop = FALSE]
    if(ncol(votaciones_data) == 0) {
      cat("WARNING: No voting columns left for", ds_info$name, "- Skipping.\n")
      next
    }
    for (col_idx in 1:ncol(votaciones_data)) {
      votaciones_data[[col_idx]] <- suppressWarnings(as.numeric(as.character(votaciones_data[[col_idx]])))
    }
    # Remove columns/rows that are all NA after coercion? Optional but might help ideal()
    # votaciones_data <- votaciones_data[, colSums(is.na(votaciones_data)) < nrow(votaciones_data)]
    # votaciones_data <- votaciones_data[rowSums(is.na(votaciones_data)) < ncol(votaciones_data), ]
    # votantes <- votantes[rowSums(is.na(votaciones_data)) < ncol(votaciones_data)] # Ensure votantes match filtered data
    if(nrow(votaciones_data) == 0 || ncol(votaciones_data) == 0) {
      cat("WARNING: No valid data left after preprocessing for", ds_info$name, "- Skipping estimation.\n")
      next
    }
    
    # --- Run IDEAL Estimation (Combined Samples & Means) ---
    # Running in 2 Dimensions by default
    estimation_output <- run_ideal_estimation_combined(
      votaciones = votaciones_data,
      votantes = votantes,
      dataset_name = ds_info$name,
      dims = 2 # Specify 2 dimensions
    )
    
    # --- Store and Save Results ---
    # Check if the function returned the expected list structure
    if (is.list(estimation_output) && all(c("samples", "means") %in% names(estimation_output))) {
      
      ideal_output_samples <- estimation_output$samples
      ideal_output_means <- estimation_output$means
      
      # Store in lists
      all_ideal_sample_results[[ds_info$name]] <- ideal_output_samples
      all_ideal_mean_results[[ds_info$name]] <- ideal_output_means
      
      # Save Samples File
      if (!is.null(ideal_output_samples) && nrow(ideal_output_samples) > 0) {
        output_filename_samples <- paste0("data - pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_", ds_info$output_suffix, "_samples.csv")
        cat("Saving full MCMC sample results to:", output_filename_samples, "\n")
        fwrite(ideal_output_samples, file = output_filename_samples, row.names = FALSE)
      } else {
        cat("No MCMC sample results generated for", ds_info$name, ", skipping sample save.\n")
      }
      
      # Save Means File
      if (!is.null(ideal_output_means) && nrow(ideal_output_means) > 0) {
        output_filename_means <- paste0("data - pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_", ds_info$output_suffix, ".csv")
        cat("Saving mean position results to:", output_filename_means, "\n")
        fwrite(ideal_output_means, file = output_filename_means, row.names = FALSE)
      } else {
        cat("No mean position results generated for", ds_info$name, ", skipping mean save.\n")
      }
      
    } else {
      cat("WARNING: Estimation function did not return the expected list structure for", ds_info$name, "\n")
      # Store empty results
      all_ideal_sample_results[[ds_info$name]] <- data.table()
      all_ideal_mean_results[[ds_info$name]] <- data.table()
    }
    
  }, error = function(e){ # Catch errors during the process for one dataset
    cat("<<<< ERROR processing dataset:", ds_info$name, ">>>>\n")
    cat("Error message:", conditionMessage(e), "\n")
    # Store empty results if an error occurred anywhere in the block
    all_ideal_sample_results[[ds_info$name]] <- data.table()
    all_ideal_mean_results[[ds_info$name]] <- data.table()
  }) # End tryCatch
  
} # End loop through datasets
end_time <- Sys.time()
execution_time_parallel <- end_time - start_time
execution_time_parallel # 2.18 parallel (4.44 non-parallel) min M4 (1000)

cat("\n\n--- Analysis Complete ---\n")

# --- PARALLEL RUNNING ---------------------------------------
# ---
# --- 

# Initialize empty lists to store results
all_ideal_sample_results <- list()
all_ideal_mean_results <- list()

# Set up parallel computing (adjust cores as needed)
cl <- parallel::makeCluster(11, type = "FORK")
doParallel::registerDoParallel(cl)

# Start timing
start_time <- Sys.time()

# Parallelized processing
processed_datasets <- foreach(ds_info = datasets_info, .packages = c("data.table")) %dopar% {
  # Initialize outputs
  result <- list(
    name = ds_info$name,
    samples = NULL,
    means = NULL,
    status = "success",
    message = ""
  )
  
  tryCatch({
    # --- Load and Preprocess Data ---
    votaciones_df <- read.csv(ds_info$file)
    
    # Preprocessing checks...
    if (ncol(votaciones_df) <= 1 || !(is.character(votaciones_df[[1]]) || is.factor(votaciones_df[[1]]))) {
      result$status <- "skipped"
      result$message <- "Data format issue or missing columns"
      return(result)
    }
    
    if(is.factor(votaciones_df[[1]])) votaciones_df[[1]] <- as.character(votaciones_df[[1]])
    votaciones_df <- votaciones_df[votaciones_df[[1]] != "Rojas Vade, Rodrigo", , drop = FALSE]
    
    if (nrow(votaciones_df) == 0) {
      result$status <- "skipped"
      result$message <- "No rows left after filtering"
      return(result)
    }
    
    votantes <- normalizar_nombres(as.vector(votaciones_df[[1]]))
    votaciones_data <- votaciones_df[,-1, drop = FALSE]
    
    if(ncol(votaciones_data) == 0) {
      result$status <- "skipped"
      result$message <- "No voting columns left"
      return(result)
    }
    
    # Numeric conversion
    for (col_idx in 1:ncol(votaciones_data)) {
      votaciones_data[[col_idx]] <- suppressWarnings(as.numeric(as.character(votaciones_data[[col_idx]])))
    }
    
    if(nrow(votaciones_data) == 0 || ncol(votaciones_data) == 0) {
      result$status <- "skipped"
      result$message <- "No valid data after preprocessing"
      return(result)
    }
    
    # --- Run IDEAL Estimation ---
    estimation_output <- run_ideal_estimation_combined(
      votaciones = votaciones_data,
      votantes = votantes,
      dataset_name = ds_info$name,
      dims = 2
    )
    
    # --- Store Results ---
    if (is.list(estimation_output) && all(c("samples", "means") %in% names(estimation_output))) {
      # Save files directly in worker
      if (!is.null(estimation_output$samples) && nrow(estimation_output$samples) > 0) {
        output_filename_samples <- paste0("data - pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_", ds_info$output_suffix, "_samples.csv")
        data.table::fwrite(estimation_output$samples, file = output_filename_samples, row.names = FALSE)
      }
      
      if (!is.null(estimation_output$means) && nrow(estimation_output$means) > 0) {
        output_filename_means <- paste0("data - pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_", ds_info$output_suffix, ".csv")
        data.table::fwrite(estimation_output$means, file = output_filename_means, row.names = FALSE)
      }
      
      result$samples <- estimation_output$samples
      result$means <- estimation_output$means
    }
    
  }, error = function(e) {
    result$status <<- "error"
    result$message <<- conditionMessage(e)
  })
  
  return(result)
}

# Stop cluster
parallel::stopCluster(cl)

# Process results and populate final lists
for (ds in processed_datasets) {
  if (ds$status == "success") {
    all_ideal_sample_results[[ds$name]] <- ds$samples
    all_ideal_mean_results[[ds$name]] <- ds$means
  } else {
    cat("\n=====================================================\n")
    cat("Dataset:", ds$name, "-", ds$status, "\n")
    cat("Reason:", ds$message, "\n")
    cat("=====================================================\n")
    
    all_ideal_sample_results[[ds$name]] <- data.table()
    all_ideal_mean_results[[ds$name]] <- data.table()
  }
}

# Print execution time
end_time <- Sys.time() # 2.11 min M4
cat("Parallel execution time:", capture.output(end_time - start_time), "\n")
