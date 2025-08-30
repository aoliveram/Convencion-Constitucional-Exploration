# This script estimates the ideological ordering of the Constitutional Convention members.
# It creates the objetcs ordenamiento_1D_WNOM_01-15.csv, ordenamiento_1D_WNOM_16-21.csv, etc., 
# and ordenamiento_1D_MCMC_01-15.csv, ordenamiento_1D_MCMC_16-21.csv, etc.

library(readr)
library(wnominate)
library(dplyr)

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

votantes_apellido_nombre <- votantes_apellido_nombre[-120] # quitamos rodrigo rojas

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
  legis.names = votantes_apellido_nombre,
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
  legis.names = votantes_apellido_nombre,
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
  legis.names = votantes_apellido_nombre,
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
  legis.names = votantes_apellido_nombre,
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
  legis.names = votantes_apellido_nombre,
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
  legis.names = votantes_apellido_nombre,
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
  legis.names = votantes_apellido_nombre,
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
  legis.names = votantes_apellido_nombre,
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
  legis.names = votantes_apellido_nombre,
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
  mutate(nombre_votante = votantes_apellido_nombre, n_votante = as.character(1:154)) %>%
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
  mutate(nombre_votante = votantes_apellido_nombre, n_votante = as.character(1:154)) %>%
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
  mutate(nombre_votante = votantes_apellido_nombre, n_votante = as.character(1:154)) %>%
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
  mutate(nombre_votante = votantes_apellido_nombre, n_votante = as.character(1:154)) %>%
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
  mutate(nombre_votante = votantes_apellido_nombre, n_votante = as.character(1:154)) %>%
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
  mutate(nombre_votante = votantes_apellido_nombre, n_votante = as.character(1:154)) %>%
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
  mutate(nombre_votante = votantes_apellido_nombre, n_votante = as.character(1:154)) %>%
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
  mutate(nombre_votante = votantes_apellido_nombre, n_votante = as.character(1:154)) %>%
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
  mutate(nombre_votante = votantes_apellido_nombre, n_votante = as.character(1:154)) %>%
  arrange(posicion_ideologica)
ordenamiento_1D_WNOM_107_109$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM_107_109)) #índice izq-der

write.csv(ordenamiento_1D_WNOM_107_109, file = "data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_107-109.csv", row.names = FALSE)
