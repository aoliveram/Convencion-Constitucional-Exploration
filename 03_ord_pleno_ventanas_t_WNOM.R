library(readr)
library(wnominate)
library(doParallel)
library(data.table)
library(dplyr)
library(stringr)
library(stringi)
library(ggplot2)
library(patchwork)
library(purrr)

#------------------------------------------------------------------------------
# Funciones
#------------------------------------------------------------------------------

# normalizar_nombres <- function(texto) {
#   texto_limpio <- iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT")
#   return(texto_limpio)
# }

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

parallel_bootstrap_wnom <- function(votaciones, votantes, N_votos, n_iter = 1000) {
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
# Bootstrap de Ordenamiento Político usando W-NOMINATE
#------------------------------------------------------------------------------

# ------------------------------- 01 - 15

# -------------------- bootstrap

votaciones_01_15 <-read.csv("ideological-scaling-files/votaciones_01_15.csv") # 147 votaciones
votaciones_01_15 <- votaciones_01_15[votaciones_01_15[[1]] != "Rojas Vade, Rodrigo", ]
# votantes <- normalizar_nombres(as.vector(votaciones_01_15[[1]]))
votaciones_01_15 <- votaciones_01_15[,-1]

n_votos <- length(votaciones_01_15)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(8, type = "FORK")  # Uses 6 cores (4P + 2E)
#cl <- makeCluster(12, type = "FORK") # CHPC
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_01_15_bootstrap <- parallel_bootstrap_wnom(
  votaciones_01_15,
  votantes_apellido_nombre,
  n_iter = 1000,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 1.86 min M1 (200), 3.33 min M4 (1000)

votantes_01_15_bootstrap <- ordenamiento_1D_WNOM_01_15_bootstrap$legislador[ordenamiento_1D_WNOM_01_15_bootstrap$iteracion == 1]

write.csv(ordenamiento_1D_WNOM_01_15_bootstrap,
          file = "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_01-15_bootstrap.csv",
          row.names = FALSE)

ordenamiento_1D_WNOM_01_15_bootstrap <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_01-15_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 16 - 21

# -------------------- bootstrap

votaciones_16_21 <-read.csv("ideological-scaling-files/votaciones_16_21.csv") # 182 votaciones
votaciones_16_21 <- votaciones_16_21[votaciones_16_21[[1]] != "Rojas Vade, Rodrigo", ]
# votantes <- normalizar_nombres(as.vector(votaciones_16_21[[1]]))
votaciones_16_21 <- votaciones_16_21[,-1]

n_votos <- length(votaciones_16_21)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(8, type = "FORK")  # Uses 6 cores (4P + 2E)
#cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_16_21_bootstrap <- parallel_bootstrap_wnom(
  votaciones_16_21, 
  votantes_apellido_nombre,
  n_iter = 1000,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 2.43 min M1 (200), 4.18 min M4 (1000)

votantes_16_21_bootstrap <- ordenamiento_1D_WNOM_16_21_bootstrap$legislador[ordenamiento_1D_WNOM_16_21_bootstrap$iteracion == 1]

votantes_01_15_bootstrap[!votantes_01_15_bootstrap %in% votantes_16_21_bootstrap] # Check

write.csv(ordenamiento_1D_WNOM_16_21_bootstrap, 
          file = "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_16-21_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_16_21_bootstrap <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_16-21_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 22 - 37

# -------------------- bootstrap

votaciones_22_37 <-read.csv("ideological-scaling-files/votaciones_22_37.csv") # 598 votaciones
votaciones_22_37 <- votaciones_22_37[votaciones_22_37[[1]] != "Rojas Vade, Rodrigo", ]
# votantes <- normalizar_nombres(as.vector(votaciones_22_37[[1]]))
votaciones_22_37 <- votaciones_22_37[,-1]

n_votos <- length(votaciones_22_37)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(8, type = "FORK")  # Uses 6 cores (4P + 2E)
#cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_22_37_bootstrap <- parallel_bootstrap_wnom(
  votaciones_22_37, 
  votantes_apellido_nombre,
  n_iter = 1000,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 7.67 min CHPC (200), 11.6 min M4 (1000)

votantes_22_37_bootstrap <- ordenamiento_1D_WNOM_22_37_bootstrap$legislador[ordenamiento_1D_WNOM_22_37_bootstrap$iteracion == 1]

votantes_01_15_bootstrap[!votantes_01_15_bootstrap %in% votantes_22_37_bootstrap] # Check

write.csv(ordenamiento_1D_WNOM_22_37_bootstrap, 
          file = "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_22-37_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_22_37_bootstrap <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_22-37_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 38 - 46 -- NO

# -------------------- bootstrap

votaciones_38_46 <-read.csv("ideological-scaling-files/votaciones_38_46.csv") # 51 votaciones
votaciones_38_46 <- votaciones_38_46[votaciones_38_46[[1]] != "Rojas Vade, Rodrigo", ]
# votantes <- normalizar_nombres(as.vector(votaciones_38_46[[1]]))
votaciones_38_46 <- votaciones_38_46[,-1]

n_votos <- length(votaciones_38_46)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(8, type = "FORK")  # Uses 6 cores (4P + 2E)
#cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_38_46_bootstrap <- parallel_bootstrap_wnom(
  votaciones_38_46, 
  votantes_apellido_nombre,
  n_iter = 1000,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 1.1 seg M4 (1000)

write.csv(ordenamiento_1D_WNOM_38_46_bootstrap, 
          file = "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_38-46_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_38_46_bootstrap <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_38-46_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 47 - 55 -- NO

# -------------------- bootstrap

votaciones_47_55 <-read.csv("ideological-scaling-files/votaciones_47_55.csv") # 64 votaciones
votaciones_47_55 <- votaciones_47_55[votaciones_47_55[[1]] != "Rojas Vade, Rodrigo", ]
# votantes <- normalizar_nombres(as.vector(votaciones_47_55[[1]]))
votaciones_47_55 <- votaciones_47_55[,-1]

n_votos <- length(votaciones_47_55)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(8, type = "FORK")  # Uses 6 cores (4P + 2E)
#cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_47_55_bootstrap <- parallel_bootstrap_wnom(
  votaciones_47_55, 
  votantes_apellido_nombre,
  n_iter = 1000,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 53.7 sec M4 (1000) 

write.csv(ordenamiento_1D_WNOM_47_55_bootstrap, 
          file = "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_47-55_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_47_55_bootstrap <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_47-55_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 56 - 75

# -------------------- bootstrap

votaciones_56_75 <-read.csv("ideological-scaling-files/votaciones_56_75.csv") # 899 votaciones
votaciones_56_75 <- votaciones_56_75[votaciones_56_75[[1]] != "Rojas Vade, Rodrigo", ]
# votantes <- normalizar_nombres(as.vector(votaciones_56_75[[1]]))
votaciones_56_75 <- votaciones_56_75[,-1]

n_votos <- length(votaciones_56_75)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(8, type = "FORK")  # Uses 6 cores (4P + 2E)
#cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_56_75_bootstrap <- parallel_bootstrap_wnom(
  votaciones_56_75, 
  votantes_apellido_nombre,
  n_iter = 1000,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel #  10.01 min CHPC (200), 19.37 min M4 (1000)

print(execution_time_parallel)

write.csv(ordenamiento_1D_WNOM_56_75_bootstrap, 
          file = "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_56-75_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_56_75_bootstrap <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_56-75_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 76 - 99

# -------------------- bootstrap

votaciones_76_99 <-read.csv("ideological-scaling-files/votaciones_76_99.csv") # 2182 votaciones
votaciones_76_99 <- votaciones_76_99[votaciones_76_99[[1]] != "Rojas Vade, Rodrigo", ]
# votantes <- normalizar_nombres(as.vector(votaciones_76_99[[1]]))
votaciones_76_99 <- votaciones_76_99[,-1]

n_votos <- length(votaciones_76_99)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(8, type = "FORK")  # Uses 6 cores (4P + 2E)
#cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_76_99_bootstrap <- parallel_bootstrap_wnom(
  votaciones_76_99, 
  votantes_apellido_nombre,
  n_iter = 1000,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 28.61 min CHPC (200), 44.11 min M4 (1000)

print(execution_time_parallel)

write.csv(ordenamiento_1D_WNOM_76_99_bootstrap, 
          file = "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_76-99_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_76_99_bootstrap <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_76-99_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 100 - 106

# -------------------- bootstrap

votaciones_100_106 <-read.csv("ideological-scaling-files/votaciones_100_106.csv") # 514 votaciones
votaciones_100_106 <- votaciones_100_106[votaciones_100_106[[1]] != "Rojas Vade, Rodrigo", ]
# votantes <- normalizar_nombres(as.vector(votaciones_100_106[[1]]))
votaciones_100_106 <- votaciones_100_106[,-1]

n_votos <- length(votaciones_100_106)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(8, type = "FORK")  # Uses 6 cores (4P + 2E)
#cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_100_106_bootstrap <- parallel_bootstrap_wnom(
  votaciones_100_106, 
  votantes_apellido_nombre,
  n_iter = 1000,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # 6.23 min CHPC (200), 9.56 min M4 (1000)

write.csv(ordenamiento_1D_WNOM_100_106_bootstrap, 
          file = "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_100-106_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_100_106_bootstrap <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_100-106_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

# ------------------------------- 107 - 109

# -------------------- bootstrap

votaciones_107_109 <-read.csv("ideological-scaling-files/votaciones_107_109.csv") # 72 votaciones
votaciones_107_109 <- votaciones_107_109[votaciones_107_109[[1]] != "Rojas Vade, Rodrigo", ]
# votantes <- normalizar_nombres(as.vector(votaciones_107_109[[1]]))
votaciones_107_109 <- votaciones_107_109[,-1]

n_votos <- length(votaciones_107_109)

# --- Run bootstrap ---

# Set up parallel computing for M1/M2 (4 performance cores + 2 efficiency)
cl <- makeCluster(8, type = "FORK")  # Uses 6 cores (4P + 2E)
#cl <- makeCluster(12, type = "FORK") # CHPC 
registerDoParallel(cl)

start_time <- Sys.time()
ordenamiento_1D_WNOM_107_109_bootstrap <- parallel_bootstrap_wnom(
  votaciones_107_109, 
  votantes_apellido_nombre,
  n_iter = 1000,
  N_votos = as.integer(round(n_votos * 0.7))
)
end_time <- Sys.time()

stopCluster(cl)

execution_time_parallel <- end_time - start_time
execution_time_parallel # --- failed

write.csv(ordenamiento_1D_WNOM_107_109_bootstrap, 
          file = "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_107-109_bootstrap.csv", 
          row.names = FALSE)

ordenamiento_1D_WNOM_107_109_bootstrap <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_107-109_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)

#------------------------------------------------------------------------------
# Datos totales de Ordenamiento W-NOMINATE
#------------------------------------------------------------------------------

# ------------------ Creamos dataset con todos los bootstrap

# Lista de archivos CSV a leer
archivos_csv_bootstrap <- c("ordenamiento_1D_WNOM_01-15_bootstrap.csv",
                            "ordenamiento_1D_WNOM_16-21_bootstrap.csv",
                            "ordenamiento_1D_WNOM_22-37_bootstrap.csv",
                            "ordenamiento_1D_WNOM_56-75_bootstrap.csv", 
                            "ordenamiento_1D_WNOM_76-99_bootstrap.csv",
                            "ordenamiento_1D_WNOM_100-106_bootstrap.csv")

# Función para leer y procesar cada archivo
leer_csv_boot <- function(archivo_bootstrap) {
  df <- read.csv(paste0("ideological-scaling-files/ordenamientos_pleno/", archivo_bootstrap), 
                 stringsAsFactors = FALSE, 
                 encoding = "UTF-8")
  df <- df[, 3] %>% data.frame()
  colnames(df) <- gsub("_bootstrap.csv", "", gsub("ordenamiento_1D_WNOM_", "", archivo_bootstrap))
  return(df)
}

# Leer todos los archivos
lista_bootstrap <- lapply(archivos_csv_bootstrap, leer_csv_boot)

# Combinar todos los dataframes
ordenamientos_completos <- cbind(
  Votante = rep(votantes_apellido_nombre, times = 200),
  do.call(cbind, lista_bootstrap)
)

# Función para realizar t-test entre dos columnas
realizar_t_test <- function(df, col1, col2) {
  t_result <- t.test(df[[col1]], df[[col2]], paired = TRUE)
  data.frame(
    comparacion = paste(col1, "vs", col2),
    p_valor = t_result$p.value,
    diferencia_media = t_result$estimate
  )
}

# Obtener todas las combinaciones posibles de columnas
columnas_numericas <- names(ordenamientos_completos)[-1]
combinaciones <- combn(columnas_numericas, 2, simplify = FALSE)

# Realizar t-test para todas las combinaciones (ordanamientos_completos tiene la pos 1D)
orden_votantes_t <- ordenamientos_completos %>%
  group_by(Votante) %>%
  do(bind_rows(lapply(combinaciones, function(combo) realizar_t_test(., combo[1], combo[2]))))

# ----- Cargamos posición inicial del candidato 

ordenamiento_1D_WNOM_01_15 <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_01-15.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)
ordenamiento_1D_WNOM_01_15 <- ordenamiento_1D_WNOM_01_15[order(ordenamiento_1D_WNOM_01_15$nombre_votante), ]

ordenamiento_1D_WNOM_16_21 <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_16-21.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)
ordenamiento_1D_WNOM_16_21 <- ordenamiento_1D_WNOM_16_21[order(ordenamiento_1D_WNOM_16_21$nombre_votante), ]

ordenamiento_1D_WNOM_22_37 <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_22-37.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)
ordenamiento_1D_WNOM_22_37 <- ordenamiento_1D_WNOM_22_37[order(ordenamiento_1D_WNOM_22_37$nombre_votante), ]

ordenamiento_1D_WNOM_56_75 <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_56-75.csv",
  stringsAsFactors = FALSE,  
  encoding = "UTF-8"         
)
ordenamiento_1D_WNOM_56_75 <- ordenamiento_1D_WNOM_56_75[order(ordenamiento_1D_WNOM_56_75$nombre_votante), ]

ordenamiento_1D_WNOM_76_99 <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_76-99.csv",
  stringsAsFactors = FALSE,  
  encoding = "UTF-8"         
)
ordenamiento_1D_WNOM_76_99 <- ordenamiento_1D_WNOM_76_99[order(ordenamiento_1D_WNOM_76_99$nombre_votante), ]

ordenamiento_1D_WNOM_100_106 <- read.csv(
  "ideological-scaling-files/ordenamientos_pleno/ordenamiento_1D_WNOM_100-106.csv",
  stringsAsFactors = FALSE,  
  encoding = "UTF-8"         
)
ordenamiento_1D_WNOM_100_106 <- ordenamiento_1D_WNOM_100_106[order(ordenamiento_1D_WNOM_100_106$nombre_votante), ]

# Crear una lista con los dataframes y sus respectivos períodos
lista_ordenamientos <- list(
  ordenamiento_1D_WNOM_01_15 %>% mutate(Periodo = "01-15"),
  ordenamiento_1D_WNOM_16_21 %>% mutate(Periodo = "16-21"),
  ordenamiento_1D_WNOM_22_37 %>% mutate(Periodo = "22-37"),
  ordenamiento_1D_WNOM_56_75 %>% mutate(Periodo = "56-75"),
  ordenamiento_1D_WNOM_76_99 %>% mutate(Periodo = "76-99"),
  ordenamiento_1D_WNOM_100_106 %>% mutate(Periodo = "100-106")
)

# ---- Unimos todos los dataframes en uno solo
ordenamientos_completos <- bind_rows(lista_ordenamientos)

# Cambiamos nombre de columna de nombres
ordenamientos_completos <- ordenamientos_completos %>%
  rename(Votante = nombre_votante)

# Normalizar nombres y períodos
orden_votantes_t <- orden_votantes_t %>%
  mutate(
    #Votante = str_squish(stri_trans_general(Votante, "Latin-ASCII")),
    Periodo = str_squish(sub(" .*", "", comparacion))  # Extrae el primer período de la comparación
  )

# ordenamientos_completos_2 <- ordenamientos_completos %>%
#   mutate(
#     Votante = str_squish(stri_trans_general(Votante, "Latin-ASCII")),
#     Periodo = str_squish(Periodo)
#   )
# 
orden_votantes_t <- orden_votantes_t %>%
  left_join(ordenamientos_completos, by = c("Votante", "Periodo"))

# --- Modificaciones adicionales ---

library(tidyr)

orden_votantes_t <- orden_votantes_t %>%
  separate(comparacion, into = c("Periodo1", "Periodo2"), sep = " vs ", remove = FALSE) %>%
  rename(dif_media_t = diferencia_media)

orden_votantes_t <- orden_votantes_t %>%
  left_join(
    ordenamientos_completos %>% select(Votante, Periodo, posicion_ideologica) %>%
      rename(Periodo1 = Periodo, pos_ideol_inicial = posicion_ideologica),
    by = c("Votante", "Periodo1")
  ) %>%
  left_join(
    ordenamientos_completos %>% select(Votante, Periodo, posicion_ideologica) %>%
      rename(Periodo2 = Periodo, pos_ideol_final = posicion_ideologica),
    by = c("Votante", "Periodo2")
  ) %>%
  mutate(dif_media = pos_ideol_final - pos_ideol_inicial) %>%
  select(Votante, comparacion, Periodo1, Periodo2, 
         pos_ideol_inicial, pos_ideol_final, dif_media, 
         dif_media_t, p_valor, everything())


# Guardamos
write.csv(orden_votantes_t, "ideological-scaling-files/03_orden_votantes_t.csv", row.names = FALSE)
saveRDS(orden_votantes_t, "ideological-scaling-files/03_orden_votantes_t.rds")

orden_votantes_t <- readRDS("ideological-scaling-files/03_orden_votantes_t.rds")

#------------------------------------------------------------------------------
# Grafica de comparación entre dos bloques de sesiones
#------------------------------------------------------------------------------

# # Cargamos datos
# orden_votantes_t <- read_csv("ideological-scaling-files/03_orden_votantes_t.csv")
# 
# # Objeto con toda la información para plot
# orden_votantes_t_procesado <- orden_votantes_t %>%
#   # Agregamos nuevas columnas
#   mutate(
#     posicion_inicial = posicion_ideologica,
#     posicion_final = posicion_ideologica + diferencia_media,
#     direccion_cambio = case_when(
#       diferencia_media > 0 ~ "Derecha",
#       diferencia_media < 0 ~ "Izquierda",
#       TRUE ~ "Sin cambio" # por si acaso
#     ),
#     significativo = p_valor < 0.05,
#     etiqueta_significancia = ifelse(significativo, "Significativo (p < 0.05)", "No Significativo (p >= 0.05)")
#   ) %>%
#   # Algunos más para plot
#   mutate(
#     direccion_cambio_factor = factor(direccion_cambio, levels = c("Izquierda", "Derecha")), 
#     etiqueta_significancia_factor = factor(etiqueta_significancia, levels = c("Significativo (p < 0.05)", "No Significativo (p >= 0.05)"))
#   )
# 
# # Fijamos escalas y formas para ambos plots consistentes
# common_y_scale <- scale_y_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, by = 0.5))
# common_color_scale <- scale_color_manual(
#   name = "Dirección del Cambio",
#   values = c("Izquierda" = "red", "Derecha" = "blue"),
#   na.translate = FALSE # No mostrar NA si "Sin cambio" no existe
# )
# common_shape_scale <- scale_shape_manual(
#   name = "Significancia Estadística",
#   values = c(
#     "Significativo (p < 0.05)" = 21,    # círculo 
#     "No Significativo (p >= 0.05)" = 24 # triángulo hacia arriba 
#   ),
#   drop = FALSE
# )
# common_fill_scale <- scale_fill_manual(
#   name = "Significancia Estadística",
#   values = c(
#     "Significativo (p < 0.05)" = "white", # Círculo blanco
#     "No Significativo (p >= 0.05)" = "black"  # Triángulo rojo
#   ),
#   drop = FALSE
# )
# common_guides <- guides(
#   shape = guide_legend(title = "Significancia Estadística"),
#   fill = guide_legend(title = "Significancia Estadística"),
#   color = guide_legend(title = "Dirección del Cambio")
# )
# 
# # --- Crear plot 1 (01-15 vs 16-21) ---
# 
# data_plot_1 <- orden_votantes_t_procesado %>% filter(comparacion == "01-15 vs 16-21")
# 
# plot_1 <- ggplot(data_plot_1, aes(x = reorder(Votante, posicion_inicial), y = posicion_inicial)) +
#   geom_segment(
#     aes(xend = reorder(Votante, posicion_inicial), yend = posicion_final, color = direccion_cambio_factor),
#     arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth = 0.8
#   ) +
#   geom_point(
#     aes(shape = etiqueta_significancia_factor, fill = etiqueta_significancia_factor),
#     size = 2.75, stroke = 0.5, color = "darkgrey" # Borde negro por defecto
#   ) +
#   common_y_scale +
#   common_color_scale +
#   common_shape_scale +
#   common_fill_scale +
#   common_guides +
#   labs(
#     title = "01-15 vs 16-21", # Título plot 1
#     x = NULL, # sin labels eje-x
#     y = "Posición Política"
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 11), # Título del subplot
#     axis.text.x = element_blank(),   # sin texto en eje-x
#     axis.ticks.x = element_blank(),  # quitamos marcas del eje X
#     legend.position = "none",        # la leyenda se coloca al final
#     panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2), # Grilla vertical sutil
#     panel.grid.minor.x = element_blank()
#   )
# 
# # --- Crear plot 2 (01-15 vs 76-99) ---
# 
# data_plot_2 <- orden_votantes_t_procesado %>% filter(comparacion == "01-15 vs 76-99")
# 
# plot_2 <- ggplot(data_plot_2, aes(x = reorder(Votante, posicion_inicial), y = posicion_inicial)) +
#   geom_segment(
#     aes(xend = reorder(Votante, posicion_inicial), yend = posicion_final, color = direccion_cambio_factor),
#     arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth = 0.8
#   ) +
#   geom_point(
#     aes(shape = etiqueta_significancia_factor, fill = etiqueta_significancia_factor),
#     size = 2.50, stroke = 0.5, color = "darkgrey" # Borde negro por defecto
#   ) +
#   common_y_scale +
#   common_color_scale +
#   common_shape_scale +
#   common_fill_scale +
#   common_guides +
#   labs(
#     title = "01-15 vs 76-99", # Título específico
#     x = "Votante (por posición inicial en 01-15)", # Mantenemos etiqueta eje-X
#     y = "Posición Política"
#   ) +
#   theme_minimal(base_size = 15) + # Tamaño label 'Votante' en eje-x
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold", size = 11), # Título de la subtrama
#     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), # Texto del eje-X
#     legend.position = "none",        # La leyenda se colectará al final
#     panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2), # Grilla vertical sutil
#     panel.grid.minor.x = element_blank()
#   )
# 
# # --- Combinar plots---
# 
# plot_combinado <- plot_1 / plot_2 + # El "/" apila verticalmente
#   plot_layout(guides = 'collect') + # Colecta las leyendas de abajo
#   plot_annotation( 
#     title = "Cambio en la Posición Política de Convencionales",
#     subtitle = "Comparación posición ideológica con W-Nominate, según ventanas de sesiones",
#     theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face="bold"),
#                   plot.subtitle = element_text(hjust = 0.5, size = 12))
#   ) 
# 
# print(plot_combinado)
# 
# # --- Guardamos el plot combinado ---
# ggsave("ideological-scaling-plots/cambio_posiciones_pleno_1.png", plot_combinado, width = 16, height = 10, units = "in", dpi = 600)
