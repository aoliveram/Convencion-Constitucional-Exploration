#install.packages('doParallel')
#install.packages('data.table')
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
parallel_bootstrap <- function(votaciones, votantes, n_iter = 200, N_votos = 100) {
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
resultados_bootstrap <- parallel_bootstrap(
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

write.csv(resultados_bootstrap, 
          file = "ordenamiento_1D_boostraping_wnom_parallel.csv", 
          row.names = FALSE)

# --- Calculate statistics ---
summary_stats <- resultados_bootstrap[, .(
  mean_coord1 = mean(coord1D),
  se_coord1 = sd(coord1D),
  mean_coord2 = mean(coord2D),
  se_coord2 = sd(coord2D)
), by = legislador]

# --- Runtime report ---
cat("Parallel bootstrap completed in", 
    round(difftime(end_time, start_time, units = "mins"), 1), 
    "minutes\n")
