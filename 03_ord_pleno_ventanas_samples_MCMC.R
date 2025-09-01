# --- Load Required Libraries ---
library(readr)
library(pscl)       # For ideal() function
library(wnominate)  # For rollcall() object creation
library(data.table) # For efficient data handling, reshaping, and writing
library(dplyr)      # For optional data manipulation steps if needed
library(foreach)

# --- Helper Functions ---

votantes_apellido_nombre <- c(
  "Abarca, Damaris",
  "Abarca, Jorge",
  "Achurra, Ignacio",
  "Aguilera, Tiare",
  "Alvarado, Gloria",
  "Alvarez, Julio",
  "Alvarez, Rodrigo",
  "Alvez, Amaya",
  "Ampuero, Adriana",
  "Andrade, Cristobal",
  "Galleguillos, Felix",
  "Arancibia, Jorge",
  "Arauna, Francisca",
  "Arellano, Marco",
  "Arrau, Martin",
  "Atria, Fernando",
  "Bacian, Wilfredo",
  "Baradit, Jorge",
  "Baranda, Benito",
  "Barcelo, Luis",
  "Barraza, Marcos",
  "Bassa, Jaime",
  "Botto, Miguel Angel",
  "Bown, Carol",
  "Bravo, Daniel",
  "Caamano, Francisco",
  "Antilef, Victorino",
  "Chinga, Eric",
  "Calvo, Carlos",
  "Cancino, Adriana",
  "Cantuarias, Rocio",
  "Carrillo, Alondra",
  "Castillo, Maria Trinidad",
  "Castillo, Eduardo",
  "Castro, Claudia",
  "Catrileo, Rosa",
  "Celedon, Roberto",
  "Celis, Raul",
  "Cespedes, Lorena",
  "Chahin, Fuad",
  "Cozzi, Ruggero",
  "Cretton, Eduardo",
  "Cruz, Andres",
  "Cubillos, Marcela",
  "Daza, Mauricio",
  "De la Maza, Bernardo",
  "Delgado, Aurora",
  "Dominguez, Gaspar",
  "Dorador, Cristina",
  "Fernandez, Patricio",
  "Flores, Alejandra",
  "Fontaine, Bernardo",
  "Fuchslocher, Javier",
  "Gallardo, Bessy",
  "Garin, Renato",
  "Giustinianovich, Elisa",
  "Godoy, Isabel",
  "Gomez, Claudio",
  "Gomez, Yarela",
  "Gonzalez, Dayana",
  "Gonzalez, Lidia",
  "Grandon, Giovanna",
  "Grandon, Paola",
  "Gutierrez, Hugo",
  "Harboe, Felipe",
  "Henriquez, Natalia",
  "Hoppe, Vanessa",
  "Hube, Constanza",
  "Hurtado, Ruth",
  "Hurtado, Maximiliano",
  "Caiguan, Alexis",
  "Jimenez, Luis",
  "Jofre, Alvaro",
  "Jurgensen, Harry",
  "Labbe, Bastian",
  "Labra, Patricia",
  "Labrana, Elsa",
  "Laibe, Tomas",
  "Larrain, Hernan",
  "Letelier, Margarita",
  "Linconao, Francisca",
  "Llanquileo, Natividad",
  "Logan, Rodrigo",
  "Loncon, Elisa",
  "Madriaga, Tania",
  "Mamani, Isabella",
  "Marinovic, Teresa",
  "Martin, Juan Jose",
  "Martinez, Helmuth",
  "Mayol, Luis",
  "Mella, Jeniffer",
  "Mena, Felipe",
  "Meneses, Janis",
  "Millabur, Adolfo",
  "Miranda, Valentina",
  "Monckeberg, Cristian",
  "Montealegre, Katerine",
  "Montero, Ricardo",
  "Moreno, Alfredo",
  "Munoz, Pedro",
  "Namor, Guillermo",
  "Navarrete, Geoconda",
  "Neumann, Ricardo",
  "Nunez, Nicolas",
  "Olivares, Ivanna",
  "Orellana, Matias",
  "Ossandon, Manuel",
  "Oyarzun, Maria Jose",
  "Perez, Alejandra",
  "Pinto, Malucha",
  "Politzer, Patricia",
  "Portilla, Ericka",
  "Pustilnick, Tammy",
  "Quinteros, Maria Elisa",
  "Rebolledo, Barbara",
  "Reyes, Maria Ramona",
  "Rivera, Pollyana",
  "Rivera, Maria Magdalena",
  "Roa, Giovanna",
  "Rojas, Rodrigo",
  "Royo, Manuela",
  "Saldana, Alvin",
  "Salinas, Fernando",
  "San Juan, Constanza",
  "Sanchez, Beatriz",
  "Schonhaut, Constanza",
  "Sepulveda, Barbara",
  "Sepulveda, Carolina",
  "Serey, Mariela",
  "Silva, Luciano",
  "Squella, Agustin",
  "Stingo, Daniel",
  "Tepper, Maria Angelica",
  "Tirado, Fernando",
  "Toloza, Pablo",
  "Ubilla, Maria Cecilia",
  "Uribe, Cesar",
  "Urrutia, Tatiana",
  "Valenzuela, Cesar",
  "Valenzuela, Paulina",
  "Vallejos, Loreto",
  "Vargas, Margarita",
  "Vargas, Mario",
  "Vega, Roberto",
  "Velasquez, Hernan",
  "Veloso, Paulina",
  "Vergara, Lisette",
  "Vidal, Rossana",
  "Videla, Carolina",
  "Viera, Christian",
  "Vilches, Carolina",
  "Villena, Ingrid",
  "Woldarsky, Manuel",
  "Zarate, Camila",
  "Zuniga, Luis Arturo"
)

votantes_apellido_nombre <- votantes_apellido_nombre[-120] # Remove "Rojas, Rodrigo"

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
    set.seed(1234) # SEED for reproducibility
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
  # Define the target legislator
  target_legislator <- "Marinovic, Teresa"
  
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
  list(name = "01_15", file = "scripts - files/votaciones_01_15.csv", output_suffix = "01-15"),
  list(name = "16_21", file = "scripts - files/votaciones_16_21.csv", output_suffix = "16-21"),
  list(name = "22_37", file = "scripts - files/votaciones_22_37.csv", output_suffix = "22-37"),
  list(name = "38_46", file = "scripts - files/votaciones_38_46.csv", output_suffix = "38-46"),
  list(name = "47_55", file = "scripts - files/votaciones_47_55.csv", output_suffix = "47-55"),
  list(name = "56_75", file = "scripts - files/votaciones_56_75.csv", output_suffix = "56-75"),
  list(name = "76_99", file = "scripts - files/votaciones_76_99.csv", output_suffix = "76-99"),
  list(name = "100_106", file = "scripts - files/votaciones_100_106.csv", output_suffix = "100-106"),
  list(name = "107_109", file = "scripts - files/votaciones_107_109.csv", output_suffix = "107-109")
)

# --- PARALLEL RUNNING ---------------------------------------
# ---
# --- 

# Initialize empty lists to store results
all_ideal_sample_results <- list()
all_ideal_mean_results <- list()

# Set up parallel computing (adjust cores as needed)
cl <- parallel::makeCluster(8, type = "FORK")
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
    
    #votantes <- as.vector(votaciones_df[[1]]) # normalizar_nombres(as.vector(votaciones_df[[1]]))
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
      votantes = votantes_apellido_nombre,
      dataset_name = ds_info$name,
      dims = 2
    )
    
    # --- Store Results ---
    if (is.list(estimation_output) && all(c("samples", "means") %in% names(estimation_output))) {
      # Save files directly in worker
      if (!is.null(estimation_output$samples) && nrow(estimation_output$samples) > 0) {
        output_filename_samples <- paste0("scripts - files/ordenamientos_pleno/ordenamiento_1D_MCMC_", ds_info$output_suffix, "_samples.csv")
        data.table::fwrite(estimation_output$samples, file = output_filename_samples, row.names = FALSE)
      }
      
      if (!is.null(estimation_output$means) && nrow(estimation_output$means) > 0) {
        output_filename_means <- paste0("scripts - files/ordenamientos_pleno/ordenamiento_1D_MCMC_", ds_info$output_suffix, ".csv")
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
end_time <- Sys.time() # 2.17 min M4
cat("Parallel execution time:", capture.output(end_time - start_time), "\n")
