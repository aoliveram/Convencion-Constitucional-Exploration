library(readr)
library(wnominate)
library(dplyr)

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

# -------------------- Rollcalls

# Votaciones en el pleno hasta el 14 de Agosto de 2021. 147 Votaciones
votaciones_al_14ago2021_manual_2 <-read.csv("data - pleno/votaciones_al_14ago2021_manual_2.csv") # 145 votaciones
votaciones_al_14ago2021_manual_2 <- votaciones_al_14ago2021_manual_2[votaciones_al_14ago2021_manual_2[[1]] != "Rojas Vade, Rodrigo", ]
votaciones_al_14ago2021_manual_2 <- votaciones_al_14ago2021_manual_2[,-1]
votaciones_al_14ago2021_rc <- rollcall(
  votaciones_al_14ago2021_manual_2,             
  yea = 1,
  nay = 0,
  missing = NA,
  #notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Conv. Const. 01-15"
)

votaciones_16_21 <- read_csv("scripts - files/votaciones_16_21.csv")
votaciones_16_21 <- votaciones_16_21[votaciones_16_21[[1]] != "Rojas Vade, Rodrigo", ]
votaciones_16_21 <- votaciones_16_21[,-1]
votaciones_16_21_rc <- rollcall(
  votaciones_16_21,             
  yea = 1,
  nay = 0,
  missing = NA,
  #notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Conv. Const. 16-21"
)

votaciones_22_37 <-read.csv("scripts - files/votaciones_22_37.csv") # 598 votaciones
votaciones_22_37 <- votaciones_22_37[votaciones_22_37[[1]] != "Rojas Vade, Rodrigo", ]
votaciones_22_37 <- votaciones_22_37[,-1]
votaciones_22_37_rc <- rollcall(
  votaciones_22_37,             
  yea = 1,
  nay = 0,
  missing = NA,
  #notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Conv. Const. 22-37"
)

votaciones_38_46 <- read_csv("scripts - files/votaciones_38_46.csv") # WILL FAIL
votaciones_38_46 <- votaciones_38_46[votaciones_38_46[[1]] != "Rojas Vade, Rodrigo", ]
votaciones_38_46 <- votaciones_38_46[,-1]
votaciones_38_46_rc <- rollcall(
  votaciones_38_46,             
  yea = 1,
  nay = 0,
  missing = NA,
  #notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Conv. Const. 38-46"
)

votaciones_47_55 <- read_csv("scripts - files/votaciones_47_55.csv") # WILL FAIL
votaciones_47_55 <- votaciones_47_55[votaciones_47_55[[1]] != "Rojas Vade, Rodrigo", ]
votaciones_47_55 <- votaciones_47_55[-c(151,156),]
votaciones_47_55 <- votaciones_47_55[,-1]
votaciones_47_55_rc <- rollcall(
  votaciones_47_55,             
  yea = 1,
  nay = 0,
  missing = NA,
  #notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Conv. Const. 47-55"
)

votaciones_56_75 <- read_csv("scripts - files/votaciones_56_75.csv")
votaciones_56_75 <- votaciones_56_75[votaciones_56_75[[1]] != "Rojas Vade, Rodrigo", ]
votaciones_56_75 <- votaciones_56_75[,-1]
votaciones_56_75_rc <- rollcall(
  votaciones_56_75,             
  yea = 1,
  nay = 0,
  missing = NA,
  #notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Conv. Const. 56-75"
)

votaciones_76_99 <- read_csv("scripts - files/votaciones_76_99.csv")
votaciones_76_99 <- votaciones_76_99[votaciones_76_99[[1]] != "Rojas Vade, Rodrigo", ]
votaciones_76_99 <- votaciones_76_99[,-1]
votaciones_76_99_rc <- rollcall(
  votaciones_76_99,             
  yea = 1,
  nay = 0,
  missing = NA,
  #notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Conv. Const. 76-99"
)

votaciones_100_106 <- read_csv("scripts - files/votaciones_100_106.csv")
votaciones_100_106 <- votaciones_100_106[votaciones_100_106[[1]] != "Rojas Vade, Rodrigo", ]
votaciones_100_106 <- votaciones_100_106[,-1]
votaciones_100_106_rc <- rollcall(
  votaciones_100_106,             
  yea = 1,
  nay = 0,
  missing = NA,
  #notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Conv. Const. 100-106"
)

votaciones_107_109 <- read_csv("scripts - files/votaciones_107_109.csv")
votaciones_107_109 <- votaciones_107_109[votaciones_107_109[[1]] != "Rojas Vade, Rodrigo", ]
votaciones_107_109 <- votaciones_107_109[,-1]
votaciones_107_109_rc <- rollcall(
  votaciones_107_109,             
  yea = 1,
  nay = 0,
  missing = NA,
  #notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Conv. Const. 107-109"
)

#------------------------------------------------------------------------------
# Estimación de Ordenamiento Político usando W-Nominate 
#------------------------------------------------------------------------------

# ---- 01-15

ordenamiento_WNOM_al_14ago2021 <- wnominate(
  votaciones_al_14ago2021_rc, 
  dims = 2,
  polarity = c(87,87) # anclamos en marinovic
)

ordenamiento_1D_WNOM_al_14ago2021 <- data.frame(posicion_ideologica = ordenamiento_WNOM_al_14ago2021$legislators$coord1D)
ordenamiento_1D_WNOM_al_14ago2021 <- reescalar(ordenamiento_1D_WNOM_al_14ago2021) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:154)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_al_14ago2021$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_al_14ago2021)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_al_14ago2021, file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_al_14ago2021.csv", row.names = FALSE)

# ---- 16-21

ordenamiento_WNOM_16_21 <- wnominate(
  votaciones_16_21_rc, 
  dims = 2, 
  polarity = c(87,87) # anclamos en marinovic
)

ordenamiento_1D_WNOM_16_21 <- data.frame(posicion_ideologica = ordenamiento_WNOM_16_21$legislators$coord1D)
ordenamiento_1D_WNOM_16_21 <- reescalar(ordenamiento_1D_WNOM_16_21) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:154)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_16_21$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_16_21)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_16_21, file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_16-21.csv", row.names = FALSE)

# ---- 22-37

ordenamiento_WNOM_22_37 <- wnominate(
  votaciones_22_37_rc, 
  dims = 2, 
  polarity = c(87,87) # anclamos en marinovic
)

ordenamiento_1D_WNOM_22_37 <- data.frame(posicion_ideologica = ordenamiento_WNOM_22_37$legislators$coord1D)
ordenamiento_1D_WNOM_22_37 <- reescalar(ordenamiento_1D_WNOM_22_37) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:154)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_22_37$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_22_37)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_22_37, file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_22-37.csv", row.names = FALSE)

# ---- 38-46 # WILL FAIL (minimum vote requirements)

ordenamiento_WNOM_38_46 <- wnominate(
  votaciones_38_46_rc, 
  dims = 2, 
  polarity = c(87,87) # anclamos en marinovic
)

ordenamiento_1D_WNOM_38_46 <- data.frame(posicion_ideologica = ordenamiento_WNOM_38_46$legislators$coord1D)
ordenamiento_1D_WNOM_38_46 <- reescalar(ordenamiento_1D_WNOM_38_46) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:154)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_38_46$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_38_46)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_38_46, file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_38-46.csv", row.names = FALSE)

# ---- 47-55 # WORKS BUT NOT THE BOOTSTRAP

ordenamiento_WNOM_47_55 <- wnominate(
  votaciones_47_55_rc, 
  dims = 2, 
  polarity = c(87,87) # anclamos en marinovic
)

ordenamiento_1D_WNOM_47_55 <- data.frame(posicion_ideologica = ordenamiento_WNOM_47_55$legislators$coord1D)
ordenamiento_1D_WNOM_47_55 <- reescalar(ordenamiento_1D_WNOM_47_55) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:154)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_47_55$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_47_55)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_47_55, file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_47-55.csv", row.names = FALSE)

# ---- 56-75

ordenamiento_WNOM_56_75 <- wnominate(
  votaciones_56_75_rc, 
  dims = 2, 
  polarity = c(87,87) # anclamos en marinovic
)

ordenamiento_1D_WNOM_56_75 <- data.frame(posicion_ideologica = ordenamiento_WNOM_56_75$legislators$coord1D)
ordenamiento_1D_WNOM_56_75 <- reescalar(ordenamiento_1D_WNOM_56_75) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:154)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_56_75$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_56_75)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_56_75, file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_56-75.csv", row.names = FALSE)

# ---- 76-99

ordenamiento_WNOM_76_99 <- wnominate(
  votaciones_76_99_rc, 
  dims = 2, 
  polarity = c(87,87) # anclamos en marinovic
)

ordenamiento_1D_WNOM_76_99 <- data.frame(posicion_ideologica = ordenamiento_WNOM_76_99$legislators$coord1D)
ordenamiento_1D_WNOM_76_99 <- reescalar(ordenamiento_1D_WNOM_76_99) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:154)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_76_99$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_76_99)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_76_99, file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_76-99.csv", row.names = FALSE)

# ---- 100-106

ordenamiento_WNOM_100_106 <- wnominate(
  votaciones_100_106_rc, 
  dims = 2, 
  polarity = c(87,87) # anclamos en marinovic
)

ordenamiento_1D_WNOM_100_106 <- data.frame(posicion_ideologica = ordenamiento_WNOM_100_106$legislators$coord1D)
ordenamiento_1D_WNOM_100_106 <- reescalar(ordenamiento_1D_WNOM_100_106) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:154)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_100_106$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_100_106)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_100_106, file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_100-106.csv", row.names = FALSE)

# ---- 107-109 # WILL FAIL (minimum vote requirements)

ordenamiento_WNOM_107_109 <- wnominate(
  votaciones_107_109_rc, 
  dims = 2, 
  polarity = c(87,87) # anclamos en marinovic
)

ordenamiento_1D_WNOM_107_109 <- data.frame(posicion_ideologica = ordenamiento_WNOM_107_109$legislators$coord1D)
ordenamiento_1D_WNOM_107_109 <- reescalar(ordenamiento_1D_WNOM_107_109) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:154)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_107_109$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_107_109)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_107_109, file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_107-109.csv", row.names = FALSE)
