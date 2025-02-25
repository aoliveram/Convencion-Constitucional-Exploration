
# -----------------------------------------------------------------------------
# ---- Parallel Bootstrap Function for W-Nominate ----
# -----------------------------------------------------------------------------

library(wnominate)
library(doParallel)
library(data.table)

# Set up parallel backend for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
registerDoParallel(cl)

# --- Define core functions ---
muestra_votos <- function(base_datos, N) {
  votos_muestreados <- sample(names(base_datos), N, replace = TRUE)
  base_datos[, votos_muestreados, drop = FALSE]
}

# --- Parallel bootstrap function ---
parallel_bootstrap_wnom <- function(votaciones, votantes, n_iter = 200, N_votos = 100) {
  resultados <- foreach(i = 1:n_iter, .combine = "rbind",
                        .packages = c("wnominate", "data.table"),
                        .export = c("muestra_votos", "votantes")) %dopar% {
                          set.seed(Sys.time() + i)  # Unique seed per iteration
                          
                          tryCatch({
                            # Generate bootstrap sample
                            muestras <- muestra_votos(votaciones, N_votos)
                            
                            # Create rollcall object
                            rc <- rollcall(muestras,
                                           yea = 1, nay = 0, missing = NA,
                                           legis.names = votantes,
                                           desc = "Bootstrap WNOMINATE")
                            
                            # Estimate coordinates (ensure polarity matches your data)
                            wnom <- wnominate(rc, dims = 2, polarity = c(87, 87))
                            
                            # Return results as data.table
                            data.table(
                              iteracion = i,
                              legislador = rownames(wnom$legislators),
                              coord1D = wnom$legislators$coord1D,
                              coord2D = wnom$legislators$coord2D
                            )
                          }, error = function(e) {
                            # Return empty data.table on error
                            data.table()
                          })
                        }
  
  # Filter failed iterations
  resultados[!is.na(coord1D)]
}

# --- Run bootstrap ---
start_time <- Sys.time()
ordenamiento_1D_boostraping_wnom_parallel <- parallel_bootstrap_wnom(
  votaciones_al_14ago2021, 
  votantes,
  n_iter = 200,
  N_votos = 100
)
end_time <- Sys.time()

# Cleanup
stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 5.78 mins vs 1.84 mins !

write.csv(ordenamiento_1D_boostraping_wnom_parallel, 
          file = "ordenamiento_1D_boostraping_wnom_parallel.csv", 
          row.names = FALSE)

ordenamiento_1D_boostraping_wnom_parallel <- read.csv(
  "ordenamiento_1D_boostraping_wnom_parallel.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# --- Calculate statistics ---

library(dplyr)

ordenamiento_1D_boostraping_wnom_parallel %>%
  group_by(legislador) %>%
  summarise(
    mean_coord1 = mean(coord1D),
    se_coord1 = sd(coord1D),
    mean_coord2 = mean(coord2D),
    se_coord2 = sd(coord2D)
  )

# -----------------------------------------------------------------------------
# ---- Parallel Bootstrap Function for MCMC ----
# -----------------------------------------------------------------------------

library(wnominate)
library(doParallel)
library(data.table)

# Set up parallel backend for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
registerDoParallel(cl)

# --- Define core functions ---
muestra_votos <- function(base_datos, N) {
  votos_muestreados <- sample(names(base_datos), N, replace = TRUE)
  base_datos[, votos_muestreados, drop = FALSE]
}

parallel_bootstrap_ideal <- function(votaciones, votantes, n_iter = 200, 
                                     N_votos = 100, maxiter = 8000, 
                                     burnin = 1000, thin = 40) {
  
  foreach(i = 1:n_iter, .combine = "rbind",
          .packages = c("pscl", "data.table"),
          .export = c("muestra_votos", "votantes")) %dopar% {
            
            set.seed(Sys.time() + i)  # Unique seed per iteration
            
            tryCatch({
              # Generate bootstrap sample
              muestras <- muestra_votos(votaciones, N_votos)
              
              # Create rollcall object
              rc <- rollcall(muestras,
                             yea = 1, nay = 0, missing = NA,
                             legis.names = votantes,
                             desc = "Bootstrap IDEAL")
              
              # Run IDEAL with conservative parameters
              ideal_fit <- pscl::ideal(rc,
                                       burnin = burnin,
                                       maxiter = maxiter,
                                       thin = thin,
                                       normalize = TRUE,
                                       verbose = FALSE)
              
              # Extract and format results
              data.table(
                iteracion = i,
                legislador = rownames(ideal_fit$xbar),
                coord1D = ideal_fit$xbar[,1],
                se1D = apply(ideal_fit$x[,,1], 1, sd)  # Posterior SD
              )
              
            }, error = function(e) {
              data.table()  # Return empty on error
            })
          }
}

# ---- Execution ----
start_time <- Sys.time()
ordenamiento_1D_boostraping_MCMC_parallel <- parallel_bootstrap_ideal(
  votaciones_al_14ago2021,
  votantes,
  n_iter = 200,
  N_votos = 100
)
end_time <- Sys.time()

# Cleanup
stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 22.93 mins vs 5.93 mins !

write.csv(ordenamiento_1D_boostraping_MCMC_parallel, 
          file = "ordenamiento_1D_boostraping_MCMC_parallel.csv", 
          row.names = FALSE)

ordenamiento_1D_boostraping_MCMC_parallel <- read.csv(
  "ordenamiento_1D_boostraping_MCMC_parallel.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# --- Calculate statistics ---

library(dplyr)

ordenamiento_1D_boostraping_MCMC_parallel %>%
  group_by(legislador) %>%
  summarise(
    mean_coord1 = -mean(coord1D),
    se_coord1 = sd(coord1D)
  )
