library(readr)
library(wnominate)
library(doParallel)
library(data.table)
library(dplyr)

normalizar_nombres <- function(texto) {
  texto_limpio <- iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT")
  return(texto_limpio)
}

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

validar_unicidad <- function(rc_object) {
  stopifnot(length(unique(rownames(rc_object$votes))) == nrow(rc_object$votes))
}

parallel_bootstrap_wnom <- function(votaciones, votantes, N_votos, n_iter = 200) {
  resultados <- foreach(i = 1:n_iter, .combine = "rbind",
                        .packages = c("wnominate", "data.table"),
                        .export = c("muestra_votos")) %dopar% {
                          set.seed(Sys.time() + i)  # Unique seed per iteration
                          
                          tryCatch({
                            # Generate bootstrap sample
                            muestras <- muestra_votos(votaciones, N_votos)
                            
                            # Create rollcall object
                            rc <- rollcall(muestras,
                                           yea = 1, nay = 0, missing = NA,
                                           legis.names = votantes,
                                           #notInLegis = NULL,
                                           desc = "Bootstrap WNOMINATE")
                            
                            validar_unicidad(rc)
                            
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
  #resultados[!is.na(coord1D)]
  resultados
}

#------------------------------------------------------------------------------
# Estimación de Ordenamiento Político usando W-NOMINATE
#------------------------------------------------------------------------------

# ------------------------------- al 14ago2021

# -------------------- bootstrap

votaciones_al_14ago2021_manual_2 <- read.csv("data - pleno/votaciones_al_14ago2021_manual_2.csv") # 145 votaciones
votaciones_al_14ago2021_manual_2 <- votaciones_al_14ago2021_manual_2[votaciones_al_14ago2021_manual_2[[1]] != "Rojas Vade, Rodrigo", ]
votantes <- normalizar_nombres(as.vector(votaciones_al_14ago2021_manual_2[[1]]))
votaciones_al_14ago2021_manual_2 <- votaciones_al_14ago2021_manual_2[,-1]

n_votos <- length(votaciones_al_14ago2021_manual_2)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
#cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_al_14ago2021_bootstrap <- parallel_bootstrap_wnom(
  votaciones_al_14ago2021_manual_2, 
  votantes,
  n_iter = 200,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 1.86 min CHPC

votantes_al_14ago2021_bootstrap <- ordenamiento_1D_WNOM_al_14ago2021_bootstrap$legislador[ordenamiento_1D_WNOM_al_14ago2021_bootstrap$iteracion == 1]

write.csv(ordenamiento_1D_WNOM_al_14ago2021_bootstrap, 
          file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_al-14ago2021_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_al_14ago2021_bootstrap <- read.csv(
  "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_al-14ago2021_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 16 - 21

# -------------------- bootstrap

votaciones_16_21 <-read.csv("scripts - files/votaciones_16_21.csv") # 183 votaciones
votaciones_16_21 <- votaciones_16_21[votaciones_16_21[[1]] != "Rojas Vade, Rodrigo", ]
votantes <- normalizar_nombres(as.vector(votaciones_16_21[[1]]))
votaciones_16_21 <- votaciones_16_21[,-1]

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
#cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_16_21_bootstrap <- parallel_bootstrap_wnom(
  votaciones_16_21, 
  votantes,
  n_iter = 200,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 2.43 min M1

votantes_16_21_bootstrap <- ordenamiento_1D_WNOM_16_21_bootstrap$legislador[ordenamiento_1D_WNOM_16_21_bootstrap$iteracion == 1]

votantes_al_14ago2021_bootstrap[!votantes_al_14ago2021_bootstrap %in% votantes_16_21_bootstrap] # Check

write.csv(ordenamiento_1D_WNOM_16_21_bootstrap, 
          file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_16-21_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_16_21_bootstrap <- read.csv(
  "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_16-21_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 22 - 37

# -------------------- bootstrap

votaciones_22_37 <-read.csv("scripts - files/votaciones_22_37.csv") # 598 votaciones
votaciones_22_37 <- votaciones_22_37[votaciones_22_37[[1]] != "Rojas Vade, Rodrigo", ]
votantes <- normalizar_nombres(as.vector(votaciones_22_37[[1]]))
votaciones_22_37 <- votaciones_22_37[,-1]

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
#cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_22_37_bootstrap <- parallel_bootstrap_wnom(
  votaciones_22_37, 
  votantes,
  n_iter = 200,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 7.67 min CHPC

votantes_22_37_bootstrap <- ordenamiento_1D_WNOM_22_37_bootstrap$legislador[ordenamiento_1D_WNOM_22_37_bootstrap$iteracion == 1]

votantes_al_14ago2021_bootstrap[!votantes_al_14ago2021_bootstrap %in% votantes_22_37_bootstrap] # Check

write.csv(ordenamiento_1D_WNOM_22_37_bootstrap, 
          file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_22-37_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_22_37_bootstrap <- read.csv(
  "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_22-37_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 38 - 46

# -------------------- bootstrap

votaciones_38_46 <-read.csv("scripts - files/votaciones_38_46.csv") # 51 votaciones
votaciones_38_46 <- votaciones_38_46[votaciones_38_46[[1]] != "Rojas Vade, Rodrigo", ]
votantes <- normalizar_nombres(as.vector(votaciones_38_46[[1]]))
votaciones_38_46 <- votaciones_38_46[,-1]

n_votos <- length(votaciones_38_46)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
#cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_38_46_bootstrap <- parallel_bootstrap_wnom(
  votaciones_38_46, 
  votantes,
  n_iter = 200,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # --- failed

write.csv(ordenamiento_1D_WNOM_38_46_bootstrap, 
          file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_38-46_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_38_46_bootstrap <- read.csv(
  "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_38-46_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 47 - 55

# -------------------- bootstrap

votaciones_47_55 <-read.csv("scripts - files/votaciones_47_55.csv") # 64 votaciones
votaciones_47_55 <- votaciones_47_55[votaciones_47_55[[1]] != "Rojas Vade, Rodrigo", ]
votantes <- normalizar_nombres(as.vector(votaciones_47_55[[1]]))
votaciones_47_55 <- votaciones_47_55[,-1]

n_votos <- length(votaciones_47_55)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
#cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_47_55_bootstrap <- parallel_bootstrap_wnom(
  votaciones_47_55, 
  votantes,
  n_iter = 200,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # --- failed

write.csv(ordenamiento_1D_WNOM_47_55_bootstrap, 
          file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_47-55_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_47_55_bootstrap <- read.csv(
  "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_47-55_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 56 - 75

# -------------------- bootstrap

votaciones_56_75 <-read.csv("scripts - files/votaciones_56_75.csv") # 899 votaciones
votaciones_56_75 <- votaciones_56_75[votaciones_56_75[[1]] != "Rojas Vade, Rodrigo", ]
votantes <- normalizar_nombres(as.vector(votaciones_56_75[[1]]))
votaciones_56_75 <- votaciones_56_75[,-1]

n_votos <- length(votaciones_56_75)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
#cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_56_75_bootstrap <- parallel_bootstrap_wnom(
  votaciones_56_75, 
  votantes,
  n_iter = 200,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel #  10.01 min CHPC

write.csv(ordenamiento_1D_WNOM_56_75_bootstrap, 
          file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_56-75_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_56_75_bootstrap <- read.csv(
  "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_56-75_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 76 - 99

# -------------------- bootstrap

votaciones_76_99 <-read.csv("scripts - files/votaciones_76_99.csv") # 2182 votaciones
votaciones_76_99 <- votaciones_76_99[votaciones_76_99[[1]] != "Rojas Vade, Rodrigo", ]
votantes <- normalizar_nombres(as.vector(votaciones_76_99[[1]]))
votaciones_76_99 <- votaciones_76_99[,-1]

n_votos <- length(votaciones_76_99)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
#cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_76_99_bootstrap <- parallel_bootstrap_wnom(
  votaciones_76_99, 
  votantes,
  n_iter = 200,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 28.61 min CHPC

write.csv(ordenamiento_1D_WNOM_76_99_bootstrap, 
          file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_76-99_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_76_99_bootstrap <- read.csv(
  "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_76-99_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 100 - 106

# -------------------- bootstrap

votaciones_100_106 <-read.csv("scripts - files/votaciones_100_106.csv") # 514 votaciones
votaciones_100_106 <- votaciones_100_106[votaciones_100_106[[1]] != "Rojas Vade, Rodrigo", ]
votantes <- normalizar_nombres(as.vector(votaciones_100_106[[1]]))
votaciones_100_106 <- votaciones_100_106[,-1]

n_votos <- length(votaciones_100_106)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
#cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_100_106_bootstrap <- parallel_bootstrap_wnom(
  votaciones_100_106, 
  votantes,
  n_iter = 200,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 6.23 min CHPC

write.csv(ordenamiento_1D_WNOM_100_106_bootstrap, 
          file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_100-106_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_100_106_bootstrap <- read.csv(
  "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_100-106_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 107 - 109

# -------------------- bootstrap

votaciones_107_109 <-read.csv("scripts - files/votaciones_107_109.csv") # 72 votaciones
votaciones_107_109 <- votaciones_107_109[votaciones_107_109[[1]] != "Rojas Vade, Rodrigo", ]
votantes <- normalizar_nombres(as.vector(votaciones_107_109[[1]]))
votaciones_107_109 <- votaciones_107_109[,-1]

n_votos <- length(votaciones_107_109)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
#cl <- makeCluster(6, type = "FORK")  # Uses 6 cores (4P + 2E)
cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_107_109_bootstrap <- parallel_bootstrap_wnom(
  votaciones_107_109, 
  votantes,
  n_iter = 200,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # --- failed

write.csv(ordenamiento_1D_WNOM_107_109_bootstrap, 
          file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_107-109_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_107_109_bootstrap <- read.csv(
  "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_107-109_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)
