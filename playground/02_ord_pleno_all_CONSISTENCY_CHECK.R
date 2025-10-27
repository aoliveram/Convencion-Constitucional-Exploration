# This script estimates the ideological ordering of the Constitutional Convention members.
# It creates the objetcs ordenamiento_1D_WNOM_01-15.csv, ordenamiento_1D_WNOM_16-21.csv, etc., 
# and ordenamiento_1D_MCMC_01-15.csv, ordenamiento_1D_MCMC_16-21.csv, etc.

library(readr)
library(wnominate)
library(dplyr)

# ================ ANÁLISIS DE ROBUSTEZ ========================================

# ================ SIN QUITAR A ROJAS, RODRIGO =================================

# Cargamos votantes
votantes <- readRDS("scripts - files/01_votantes.rds")
#votantes <- votantes[-120] # quitamos rodrigo rojas

# Reescalamos dentro de [-1,1]
reescalar <- function(vector_original) {
  min_original <- min(vector_original)
  max_original <- max(vector_original)
  a <- -1
  b <- 1
  vector_reescalado <- ((vector_original - min_original) / (max_original - min_original)) * (b - a) + a
  return(vector_reescalado)
}

votaciones_01_15 <-read.csv("scripts - files/votaciones_01_15.csv") # 145 votaciones
#votaciones_01_15 <- votaciones_01_15[votaciones_01_15[[1]] != "Rojas Vade, Rodrigo", ]
votaciones_01_15 <- votaciones_01_15[,-1]
votaciones_01_15_rc <- rollcall(
  votaciones_01_15,             
  yea = 1,
  nay = 0,
  missing = NA,
  #notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Conv. Const. 01-15"
)

ordenamiento_WNOM_01_15 <- wnominate(
  votaciones_01_15_rc, 
  dims = 2,
  polarity = c(87,87) # anclamos en marinovic
)

ordenamiento_1D_WNOM_01_15 <- data.frame(posicion_ideologica = ordenamiento_WNOM_01_15$legislators$coord1D)
ordenamiento_1D_WNOM_01_15 <- reescalar(ordenamiento_1D_WNOM_01_15) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:155)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_01_15$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_01_15)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_01_15, file = "scripts - files/ordenamientos_pleno/ordenamiento_1D_WNOM_01_15_155.csv", row.names = FALSE)

ordenamiento_1D_WNOM_01_15_155 <- read_csv("scripts - files/ordenamientos_pleno/ordenamiento_1D_WNOM_01_15_155.csv")

# ================ CON QUITAR A ROJAS, RODRIGO =================================

# Cargamos votantes
votantes <- readRDS("scripts - files/01_votantes.rds")
votantes <- votantes[-120] # quitamos rodrigo rojas

# Reescalamos dentro de [-1,1]
reescalar <- function(vector_original) {
  min_original <- min(vector_original)
  max_original <- max(vector_original)
  a <- -1
  b <- 1
  vector_reescalado <- ((vector_original - min_original) / (max_original - min_original)) * (b - a) + a
  return(vector_reescalado)
}

votaciones_01_15 <-read.csv("scripts - files/votaciones_01_15.csv") # 145 votaciones
votaciones_01_15 <- votaciones_01_15[votaciones_01_15[[1]] != "Rojas Vade, Rodrigo", ]
votaciones_01_15 <- votaciones_01_15[,-1]
votaciones_01_15_rc <- rollcall(
  votaciones_01_15,             
  yea = 1,
  nay = 0,
  missing = NA,
  #notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Conv. Const. 01-15"
)

ordenamiento_WNOM_01_15 <- wnominate(
  votaciones_01_15_rc, 
  dims = 2,
  polarity = c(87,87) # anclamos en marinovic
)

ordenamiento_1D_WNOM_01_15 <- data.frame(posicion_ideologica = ordenamiento_WNOM_01_15$legislators$coord1D)
ordenamiento_1D_WNOM_01_15 <- reescalar(ordenamiento_1D_WNOM_01_15) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:154)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_01_15$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_01_15)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_01_15, file = "scripts - files/ordenamientos_pleno/ordenamiento_1D_WNOM_01_15_154.csv", row.names = FALSE)

ordenamiento_1D_WNOM_01_15_154 <- read_csv("scripts - files/ordenamientos_pleno/ordenamiento_1D_WNOM_01_15_154.csv")

# ================= FIN ANÁLISIS DE ROBUSTEZ ===================================
