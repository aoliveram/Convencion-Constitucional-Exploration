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
    if(is.factor(votaciones_df[[1]])) {votaciones_df[[1]] <- as.character(votaciones_df[[1]])}
    votaciones_df <- votaciones_df[votaciones_df[[1]] != "Rojas Vade, Rodrigo", , drop = FALSE]
    if (nrow(votaciones_df) == 0) {
      cat("WARNING: No rows left after filtering for", ds_info$name, "- Skipping.\n")
      next
    }
    #votantes <- as.vector(votaciones_df[[1]]) # normalizar_nombres(as.vector(votaciones_df[[1]]))
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
      votantes = votantes_apellido_nombre,
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
