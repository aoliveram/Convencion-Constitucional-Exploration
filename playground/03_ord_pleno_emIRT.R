# --- Load Required Libraries ---
library(MCMCpack)   # For MCMCdynamicIRT1d
library(data.table) # For efficient data handling
library(stringi)    # For robust string transliteration
library(purrr)      # For functional programming (map_df)

# --- Helper Functions ---
normalizar_nombres <- function(texto) {
  texto_limpio <- stri_trans_general(texto, "Latin-ASCII")
  # Consider lowercasing and trimming for maximum robustness
  # texto_limpio <- str_trim(tolower(texto_limpio))
  return(texto_limpio)
}

# --- Configuration ---

periods_to_load <- c("01_15", "16_21", "22_37")
base_path <- "data - pleno/ordenamientos_pleno/" # Adjust if needed
input_file_pattern <- "data - pleno/votaciones_{period}.csv"

# --- 1. Load Data and Prepare for MCMCdynamicIRT1d ---
cat("Loading and preparing data...\n")

all_votes_list <- list()
all_legislators_list <- list()
item_time_map_list <- list()
current_period_index <- 1 # Start period indexing at 1

for (period in periods_to_load) {
  filename <- file.path(dirname(base_path), sub("\\{period\\}", period, basename(input_file_pattern)))
  
  if (file.exists(filename)) {
    cat("Reading:", filename, "\n")
    df <- read.csv(filename, stringsAsFactors = FALSE, encoding = "UTF-8", check.names = FALSE) # check.names=FALSE important if vote names have special chars
    
    # Basic validation
    if (ncol(df) < 2 || !(is.character(df[[1]]) || is.factor(df[[1]]))) {
      cat("WARNING: Skipping file", filename, "- unexpected format.\n")
      next
    }
    
    # Preprocessing
    if(is.factor(df[[1]])) df[[1]] <- as.character(df[[1]])
    df <- df[df[[1]] != "Rojas Vade, Rodrigo", , drop = FALSE]
    if (nrow(df) == 0) {
      cat("WARNING: No rows left after filtering", filename, "- Skipping period.\n")
      next
    }
    df[[1]] <- normalizar_nombres(df[[1]])
    legislators <- df[[1]]
    votes_matrix <- as.matrix(df[, -1, drop = FALSE]) # Votes start from col 2
    
    # Convert votes to numeric 0/1/NA (MCMCdynamicIRT1d expects 0/1/NA)
    suppressWarnings(storage.mode(votes_matrix) <- "numeric") # Coerce to numeric
    
    # Check for valid data
    if(ncol(votes_matrix) == 0 || nrow(votes_matrix) == 0) {
      cat("WARNING: No valid data after processing for period", period, ". Skipping.\n")
      next
    }
    
    # Store results for this period
    all_votes_list[[period]] <- votes_matrix
    all_legislators_list[[period]] <- legislators
    # Create time map: repeat current period index for each vote in this period
    item_time_map_list[[period]] <- rep(current_period_index, ncol(votes_matrix))
    
    current_period_index <- current_period_index + 1
    
  } else {
    cat("WARNING: File not found, skipping:", filename, "\n")
  }
}

if (length(all_votes_list) < 2) {
  stop("ERROR: Need at least two periods of data to run a dynamic model.")
}

# --- Combine data into the required format ---

# Master list of all unique legislators across all loaded periods, sorted
all_legislators_sorted <- sort(unique(unlist(all_legislators_list)))
n_legislators_total <- length(all_legislators_sorted)
cat("Total unique legislators across selected periods:", n_legislators_total, "\n")

# Combine vote matrices and create the final Y matrix (Votes x Legislators)
combined_Y_list <- list()
final_item_time_map <- integer(0)
n_periods_actual <- length(all_votes_list)

for (i in 1:n_periods_actual) {
  period_name <- names(all_votes_list)[i]
  period_votes <- all_votes_list[[period_name]]
  period_legislators <- all_legislators_list[[period_name]]
  period_time_index <- i # Use the loop index (1-based)
  
  # Create a matrix for this period with ALL legislators as columns
  n_votes_period <- ncol(period_votes)
  period_Y <- matrix(NA, nrow = n_votes_period, ncol = n_legislators_total)
  colnames(period_Y) <- all_legislators_sorted
  
  # Find the column indices for legislators present in this period
  present_indices <- match(period_legislators, all_legislators_sorted)
  
  # Fill the matrix - **IMPORTANT: TRANSPOSE needed**
  # MCMCdynamicIRT1d wants Items(Votes) x Legislators
  # Our period_votes is Legislators x Votes, so we transpose it first
  period_votes_transposed <- t(period_votes) # Now Votes x Legislators_in_period
  period_Y[, present_indices] <- period_votes_transposed
  
  combined_Y_list[[i]] <- period_Y
  final_item_time_map <- c(final_item_time_map, rep(period_time_index, n_votes_period))
}

# Combine all period matrices vertically
Y_matrix <- do.call(rbind, combined_Y_list)
cat("Final data matrix Y dimensions (Votes x Legislators):", dim(Y_matrix), "\n")
cat("Length of item.time.map:", length(final_item_time_map), "\n")
stopifnot(nrow(Y_matrix) == length(final_item_time_map)) # Validation

# --- 2. Set up Constraints and Run MCMCdynamicIRT1d ---
cat("\nSetting up constraints and running MCMCdynamicIRT1d...\n")

# Define constraints for identification (REQUIRED)
# Choose two legislators expected to be consistently at opposite poles
# **You MUST choose legislators from 'all_legislators_sorted'**
# Example: (Replace with actual names relevant to your context)
# Find likely poles from a preliminary static analysis or prior knowledge
constraint_legislator_neg <- all_legislators_sorted[1] # Example: first alphabetically
constraint_legislator_pos <- all_legislators_sorted[length(all_legislators_sorted)] # Example: last alphabetically

# Create the constraint list using the *exact* names
theta_constraints <- list()
theta_constraints[[constraint_legislator_neg]] <- "-"
theta_constraints[[constraint_legislator_pos]] <- "+"

cat("Using constraints:\n")
print(theta_constraints)

# MCMC Settings (adjust for real analysis)
n_iter_mcmc <- 5000  # Reduced for example speed
n_burn_mcmc <- 1000
n_thin_mcmc <- 10     # Keep every 10th -> (5000-1000)/10 = 400 samples
mcmc_verbose <- 500

# Run the model
start_time_mcmc <- Sys.time()
mcmc_result <- NULL
tryCatch({
  mcmc_result <- MCMCdynamicIRT1d(
    datamatrix = Y_matrix,               # Votes x Legislators matrix
    item.time.map = final_item_time_map, # Map votes to time periods (1-based index)
    mcmc = n_iter_mcmc,
    burnin = n_burn_mcmc,
    thin = n_thin_mcmc,
    verbose = mcmc_verbose,
    store.item = FALSE,                # Don't store item params for now
    theta.constraints = theta_constraints # Apply identification constraints
    # Keep other priors as default for this example
  )
}, error = function(e){
  cat("ERROR running MCMCdynamicIRT1d:", conditionMessage(e), "\n")
})
end_time_mcmc <- Sys.time()
cat("MCMCdynamicIRT1d estimation took:", end_time_mcmc - start_time_mcmc, "\n")


# --- 3. Extract and Process Results ---
if (!is.null(mcmc_result)) {
  cat("\nProcessing MCMC results...\n")
  
  # The result is an mcmc object. Convert to matrix to access columns by name.
  mcmc_matrix <- as.matrix(mcmc_result)
  mcmc_colnames <- colnames(mcmc_matrix)
  
  # Identify theta columns (ideal points) - format is typically "theta.T.L"
  # T = Time period index (1 to n_periods_actual)
  # L = Legislator index (1 to n_legislators_total)
  theta_cols <- grep("^theta\\.[0-9]+\\.[0-9]+$", mcmc_colnames, value = TRUE)
  
  if (length(theta_cols) > 0) {
    # Extract only the theta samples
    theta_samples_matrix <- mcmc_matrix[, theta_cols, drop = FALSE]
    
    # Calculate means for each theta.T.L
    theta_means <- colMeans(theta_samples_matrix)
    
    # Create a data.table to store results in long format
    results_long <- data.table(
      param_name = names(theta_means),
      mean_position = theta_means
    )
    
    # Extract Time (T) and Legislator Index (L) from the parameter name
    results_long[, c("T_idx", "L_idx") := tstrsplit(str_remove(param_name, "theta\\."), "\\.", fixed = TRUE, type.convert = TRUE)]
    
    # Map indices back to names
    results_long[, Periodo := names(all_votes_list)[T_idx]] # Assumes order matches index
    results_long[, Votante := all_legislators_sorted[L_idx]]
    
    # Select and format final output (mean position per legislator per period)
    final_means_dyn <- results_long[, .(Votante, Periodo, posicion_ideologica_dyn = mean_position)]
    final_means_dyn <- final_means_dyn[order(Periodo, Votante)] # Order for readability
    
    cat("Mean dynamic ideal points extracted:\n")
    print(head(final_means_dyn))
    
    # Save the mean dynamic positions
    # fwrite(final_means_dyn, "dynamic_ideal_points_mcmcpack_means.csv")
    
    # --- To get full samples in long format (if needed, LARGE output) ---
    # Warning: This creates a very large data frame
    # theta_samples_long <- melt(as.data.table(theta_samples_matrix, keep.rownames = "iteracion"),
    #                           id.vars="iteracion", variable.name="param_name", value.name="ideal_point_sample")
    # theta_samples_long[, c("T_idx", "L_idx") := tstrsplit(str_remove(param_name, "theta\\."), "\\.", fixed = TRUE, type.convert = TRUE)]
    # theta_samples_long[, Periodo := names(all_votes_list)[T_idx]]
    # theta_samples_long[, Votante := all_legislators_sorted[L_idx]]
    # final_samples_dyn <- theta_samples_long[, .(iteracion, Votante, Periodo, ideal_point_sample)]
    # fwrite(final_samples_dyn, "dynamic_ideal_points_mcmcpack_samples.csv") # Very large file
    
  } else {
    cat("Could not find theta parameters in MCMC output.\n")
  }
  
  # You should also check convergence using coda::plot(mcmc_result) or similar diagnostics
  # plot(mcmc_result) # Might need to select specific parameters if too many
  
} else {
  cat("MCMCdynamicIRT1d estimation failed.\n")
}