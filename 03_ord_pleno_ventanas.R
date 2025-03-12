#library(readr)
#votaciones_al_14ago2021 <- read_csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv", locale = locale(encoding = "LATIN1"))
#votaciones_al_14ago2021_E <- read.csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv")
#votaciones_al_14ago2021 <- votaciones_al_14ago2021[,-1] # Quito la primera columna

# Guardo los votantes y quito columna
#votantes <- as.vector(votaciones_al_14ago2021[[1]])
#votaciones_al_14ago2021 <- votaciones_al_14ago2021[,-1]

library(readr)
library(wnominate)
library(doParallel)
library(data.table)

# Reescalamos dentro de [-1,1]
reescalar <- function(vector_original) {
  min_original <- min(vector_original)
  max_original <- max(vector_original)
  a <- -1
  b <- 1
  vector_reescalado <- ((vector_original - min_original) / (max_original - min_original)) * (b - a) + a
  return(vector_reescalado)
}

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

#------------------------------------------------------------------------------
# Estimación de Ordenamiento Político usando MCMC Bayesiano
#------------------------------------------------------------------------------

# ------------------------------- al 14ago2021

# -------------------- estimación

votaciones_al_14ago2021_manual_2 <-read.csv("Pleno/votaciones_al_14ago2021_manual_2.csv") # 145 votaciones
votantes <- as.vector(votaciones_al_14ago2021_manual_2[[1]])
votaciones_al_14ago2021_manual_2 <- votaciones_al_14ago2021_manual_2[,-1]

votaciones_al_14ago2021_rc <- rollcall(
  votaciones_al_14ago2021_manual_2,             
  yea = c(1),
  nay = c(0),
  missing = c(NA),
  notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Convención Constitucional al 14 Ago 2021"
)

ordenamiento_MCMC <- ideal(
  votaciones_al_14ago2021_rc,
  codes = votaciones_al_14ago2021_rc$codes,
  maxiter = 8000,  # Número máximo de iteraciones
  burnin = 1000,   # Número de iteraciones para el periodo de "quemado"
  thin = 40,      # Intervalo para guardar muestras
  normalize = T   # Normalizar los datos (ver documentación para más detalles)
)

ordenamiento_1D_MCMC <- ordenamiento_MCMC$xbar %>% 
  as.data.frame() %>%
  rename(posicion_ideologica = D1)
ordenamiento_1D_MCMC$posicion_ideologica <- -ordenamiento_1D_MCMC$posicion_ideologica

ordenamiento_1D_MCMC <- reescalar(ordenamiento_1D_MCMC) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:155)) %>%
  arrange(posicion_ideologica)

rownames(ordenamiento_1D_MCMC) <- NULL
ordenamiento_1D_MCMC$posicion_izq_der <- c(1:155)

write.csv(ordenamiento_1D_MCMC, 
          file = "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_14-07_14-08.csv", 
          row.names = FALSE)

# -------------------- bootstrap

votaciones_al_14ago2021_manual_2 <-read.csv("Pleno/votaciones_al_14ago2021_manual_2.csv") # __ votaciones
votantes <- as.vector(votaciones_al_14ago2021_manual_2[[1]])
votaciones_al_14ago2021_manual_2 <- votaciones_al_14ago2021_manual_2[,-1]

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_boostraping_MCMC_parallel <- parallel_bootstrap_ideal(
  votaciones_al_14ago2021_manual_2,
  votantes,
  n_iter = 200,
  N_votos = 100
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel  # 5.88 min

write.csv(ordenamiento_1D_boostraping_MCMC_parallel, 
          file = "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_al-14ago2021_parallel.csv", 
          row.names = FALSE)

ordenamiento_1D_boostraping_MCMC_parallel <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_al-14ago2021_parallel.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# -------------------------------  16 - 21

# -------------------- bootstrap

votaciones_16_21 <-read.csv("Pleno/votaciones_16_21.csv") # 182 votaciones
votantes <- as.vector(votaciones_16_21[[1]])
votaciones_16_21 <- votaciones_16_21[,-1]

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(7, type = "FORK")  # Uses 6 cores (4P + 2E)
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_boostraping_MCMC_parallel <- parallel_bootstrap_ideal(
  votaciones_16_21,
  votantes,
  n_iter = 200,
  N_votos = 100
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel  # 5.65 min

write.csv(ordenamiento_1D_boostraping_MCMC_parallel, 
          file = "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_16-21_parallel.csv", 
          row.names = FALSE)

ordenamiento_1D_boostraping_MCMC_parallel <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_16-21_parallel.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# -------------------------------  22 - 37

# -------------------- bootstrap

votaciones_22_37 <-read.csv("Pleno/votaciones_22_37.csv") # -- votaciones
votantes <- as.vector(votaciones_22_37[[1]])
votaciones_22_37 <- votaciones_22_37[,-1]

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(7, type = "FORK")  # Uses 6 cores (4P + 2E)
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_boostraping_MCMC_parallel <- parallel_bootstrap_ideal(
  votaciones_22_37,
  votantes,
  n_iter = 200,
  N_votos = 100
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel  # 5.34 min

write.csv(ordenamiento_1D_boostraping_MCMC_parallel, 
          file = "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_22-37_parallel.csv", 
          row.names = FALSE)

ordenamiento_1D_boostraping_MCMC_parallel <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_22-37_parallel.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# -------------------------------  38 - 46

# -------------------- bootstrap

votaciones_38_46 <-read.csv("Pleno/votaciones_38_46.csv") # 51 votaciones
votantes <- as.vector(votaciones_38_46[[1]])
votaciones_38_46 <- votaciones_38_46[,-1]

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(7, type = "FORK")  # Uses 6 cores (4P + 2E)
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_boostraping_MCMC_parallel <- parallel_bootstrap_ideal(
  votaciones_38_46,
  votantes,
  n_iter = 200,
  N_votos = 100
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel  # 4.21 min

write.csv(ordenamiento_1D_boostraping_MCMC_parallel, 
          file = "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_38-46_parallel.csv", 
          row.names = FALSE)

ordenamiento_1D_boostraping_MCMC_parallel <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_38-46_parallel.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# -------------------------------  47 - 55

# -------------------- bootstrap

votaciones_47_55 <-read.csv("Pleno/votaciones_47_55.csv") # 51 votaciones
votantes <- as.vector(votaciones_47_55[[1]])
votaciones_47_55 <- votaciones_47_55[,-1]

#### ----

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(7, type = "FORK")  # Uses 6 cores (4P + 2E)
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_boostraping_MCMC_parallel <- parallel_bootstrap_ideal(
  votaciones_47_55,
  votantes,
  n_iter = 200,
  N_votos = 100
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel  # 5.551 min (not plugged)

write.csv(ordenamiento_1D_boostraping_MCMC_parallel, 
          file = "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_47-55_parallel.csv", 
          row.names = FALSE)

ordenamiento_1D_boostraping_MCMC_parallel <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_47-55_parallel.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# -------------------------------  56 - 75

# -------------------- bootstrap

votaciones_56_75 <-read.csv("Pleno/votaciones_56_75.csv") # -- votaciones
votantes <- as.vector(votaciones_56_75[[1]])
votaciones_56_75 <- votaciones_56_75[,-1]

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(7, type = "FORK")  # Uses 6 cores (4P + 2E)
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_boostraping_MCMC_parallel <- parallel_bootstrap_ideal(
  votaciones_56_75,
  votantes,
  n_iter = 200,
  N_votos = 100
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel  # -- min

write.csv(ordenamiento_1D_boostraping_MCMC_parallel, 
          file = "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_56-75_parallel.csv", 
          row.names = FALSE)

ordenamiento_1D_boostraping_MCMC_parallel <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_56-75_parallel.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# -------------------------------  76 - 99
# !!!!
# -------------------- bootstrap

votaciones_76_99 <-read.csv("Pleno/votaciones_76_99.csv") # -- votaciones
votantes <- as.vector(votaciones_76_99[[1]])
votaciones_76_99 <- votaciones_76_99[,-1]

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(7, type = "FORK")  # Uses 6 cores (4P + 2E)
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_boostraping_MCMC_parallel <- parallel_bootstrap_ideal(
  votaciones_76_99,
  votantes,
  n_iter = 200,
  N_votos = 100
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel  # -- min

write.csv(ordenamiento_1D_boostraping_MCMC_parallel, 
          file = "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_76-99_parallel.csv", 
          row.names = FALSE)

ordenamiento_1D_boostraping_MCMC_parallel <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_76-99_parallel.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# -------------------------------  100 - 106

# -------------------- bootstrap

votaciones_100_106 <-read.csv("Pleno/votaciones_100_106.csv") # -- votaciones
votantes <- as.vector(votaciones_100_106[[1]])
votaciones_100_106 <- votaciones_100_106[,-1]

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(7, type = "FORK")  # Uses 6 cores (4P + 2E)
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_boostraping_MCMC_parallel <- parallel_bootstrap_ideal(
  votaciones_100_106,
  votantes,
  n_iter = 200,
  N_votos = 100
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel  # -- min

write.csv(ordenamiento_1D_boostraping_MCMC_parallel, 
          file = "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_100-106_parallel.csv", 
          row.names = FALSE)

ordenamiento_1D_boostraping_MCMC_parallel <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_100-106_parallel.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# -------------------------------  107 - 109

# -------------------- bootstrap

votaciones_107_109 <-read.csv("Pleno/votaciones_107_109.csv") # -- votaciones
votantes <- as.vector(votaciones_107_109[[1]])
votaciones_107_109 <- votaciones_107_109[,-1]

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(7, type = "FORK")  # Uses 6 cores (4P + 2E)
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_boostraping_MCMC_parallel <- parallel_bootstrap_ideal(
  votaciones_107_109,
  votantes,
  n_iter = 200,
  N_votos = 100
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel  # -- min

write.csv(ordenamiento_1D_boostraping_MCMC_parallel, 
          file = "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_107-109_parallel.csv", 
          row.names = FALSE)

ordenamiento_1D_boostraping_MCMC_parallel <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_107-109_parallel.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)
