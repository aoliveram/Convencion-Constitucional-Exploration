# Load necessary libraries
library(readxl)
library(dplyr)

# Read the Excel file
#data <- read_excel('Comisión/25_sistema_politico.xls', col_names = FALSE)
data <- read_excel('Pleno/sesion_103.xls', col_names = FALSE)

# Identify rows where the first column contains "VOTACIONID"
start_indices <- which(data[[1]] == "VOTACIONID")

# Create a list to store blocks of data
blocks <- list()

# Loop through the start indices to separate the blocks
for (i in seq_along(start_indices)) {
  start_idx <- start_indices[i]
  end_idx <- ifelse(i < length(start_indices), start_indices[i + 1] - 1, nrow(data))
  
  # Extract the block and store it in the list
  block <- data[start_idx:end_idx, ]
  blocks[[i]] <- block
}

# Check the number of blocks created
length(blocks)

# Optional:
str(blocks)
str(blocks[[1]])


#------------------------------------------------------------------------------

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Initialize an empty list to store processed data
votaciones_data <- list()

# Iterate over each block in the 'blocks' object
for (i in seq_along(blocks)) {
  block <- blocks[[i]]
  
  # Extract VOTACIONID and FECHA from the block
  votacion_id <- block[2, 1][[1]]  # VOTACIONID is in row 2, column 1
  fecha_raw <- block[2, 3][[1]]    # FECHA is in row 2, column 4
  
  # Extract only the date portion from FECHA
  fecha <- format(as.Date(substr(fecha_raw, 1, 10), format = "%d-%m-%Y"), "%m-%d")
  
  # Extract voting data starting from row 4 onwards
  voting_data <- block[-c(1:3), ]  # Remove metadata and header rows
  
  # Assign proper column names based on the structure
  colnames(voting_data) <- c("NA1","NOMBRE", "VOTACION", "NA2", "NA3", "NA4", "NA5", "NA6", "NA7")
  
  # Select relevant columns: NOMBRE and VOTACION
  temp_data <- voting_data %>%
    select(NOMBRE, VOTACION) %>%
    mutate(
      VOTACIONID = votacion_id,
      FECHA = fecha,
      VOTACION = case_when(
        VOTACION == "AFIRMATIVO" ~ 1,
        VOTACION == "ABSTENCION" ~ 0,
        VOTACION == "EN CONTRA" ~ -1,
        VOTACION == "NO VOTA" ~ 9,
        TRUE ~ NA_real_  # Handle unexpected values
      )
    )
  
  # Append to the list
  votaciones_data[[i]] <- temp_data
}

# Combine all blocks into a single data frame
votaciones_df <- do.call(rbind, votaciones_data)

#------------------------------------------------------------------------------
# Plot
#------------------------------------------------------------------------------

# Create a heatmap plot
ggplot(votaciones_df, aes(x = factor(VOTACIONID), y = NOMBRE, fill = factor(VOTACION))) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("-1" = "red", "0" = "yellow", "1" = "green", "9" = "gray"),
    name = "Votación",
    labels = c("En Contra (-1)", "Abstención (0)", "A Favor (1)", "No Vota (9)")
  ) +
  labs(
    title = paste("Heatmap de Votaciones Sesión", votaciones_df['FECHA'][[1]][1]),
    x = "VOTACIONID",
    y = "Nombre"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7)
  ) 

#------------------------------------------------------------------------------
# Coalitions
#------------------------------------------------------------------------------

votaciones_df['NOMBRE'][[1]][1:20]

# Transform NOMBRE in votaciones_df to extract first surname and first name 
transform_votaciones_names <- function(name) {
  parts <- unlist(strsplit(name, ", "))
  if (length(parts) == 2) {
    surnames <- unlist(strsplit(parts[1], " ")) # Split surnames
    first_surname <- surnames[1]               # Take only the first surname
    first_name <- unlist(strsplit(parts[2], " "))[1] # Take only the first name
    return(paste(first_surname, ",", first_name))
  }
  return(NA)
}

# Apply transformation to NOMBRE column in votaciones_df
#votaciones_df$NOMBRE <- sapply(votaciones_df$NOMBRE, transform_votaciones_names)
nombres_transformed <- sapply(votaciones_df$NOMBRE, transform_votaciones_names)
nombres_transformed[1:20]

# Frente Amplio
selected_names_FA <- c(
  "Abarca González, Damaris", "Achurra Díaz, Ignacio", "Alvez Marín, Amaya", "Atria Lemaitre, Fernando", 
  "Baradit Morales, Jorge", "Bassa Mercado, Jaime", "Delgado Vergara, Aurora", "Gómez Sánchez, Yarela", 
  "Mella Escobar, Jeniffer", "Oyarzún Solis, María José", "Roa Cadin, Giovanna", "Sánchez Muñoz, Beatriz", 
  "Schonhaut Soto, Constanza", "Serey Jiménez, Mariela", "Stingo Camus, Daniel", "Urrutia Herrera, Tatiana", 
  "Viera Álvarez, Christian"
)

# Chile Digno
selected_names_CHD <- c(
  "Barraza Gómez, Marcos", "Celedón Fernández, Roberto", "Gallardo Prado, Bessy", "Grandón González, Paola",
  "Gutiérrez Gálvez, Hugo", "Miranda Arce, Valentina", "Núñez Gangas, Nicolás", "Portilla Barrios, Ericka",
  "Sepúlveda Hales, Bárbara", "Velásquez Núñez, Hernán", "Videla Osorio, Carolina"
)

# Coordinadora Constituyente Plurinacional y Popular
selected_names_CCPP <- c(
  "Antilef Ñanco, Victorino", "Arellano Ortega, Marco", "Bacian Delgado, Wilfredo", "Caiguan Ancapan, Alexis",
  "Chinga Ferreira, Eric", "Godoy Monardez, Isabel", "Grandón Caro, Giovanna", "Labraña Pino, Elsa",
  "Linconao Huircapán, Francisca", "Llanquileo Pilquimán, Natividad", "Madriaga Flores, Tania",
  "Olivares Miranda, Ivanna", "Pérez Espina, Alejandra", "Rivera Iribarren, María Magdalena", "Vargas López, Margarita",
  "Vergara Riquelme, Lisette", "Woldarsky González, Manuel"
)

# Colectivo Socialista
selected_names_CS <- c(
  "Alvarez Pinto, Julio", "Calvo Muñoz, Carlos", "Cancino Meneses, Adriana", "Castillo Boilet, María Trinidad",
  "Cruz Carrasco, Andrés", "Fernández Chadwick, Patricio", "Gómez Castro, Claudio", "Hurtado Roco, Maximiliano",
  "Laibe Saez, Tomás", "Montero Allende, Ricardo", "Muñoz Leiva, Pedro", "Orellana Cuellar, Matías",
  "Pinto Solari, Malucha", "Reyes Painequeo, María Ramona", "Valenzuela Maass, César", "Vargas Vidal, Mario"
)

# Independientes por una Nueva Constitución
selected_names_INC <- c(
  "Abarca Riveros, Jorge", "Baranda Ferrán, Benito", "Céspedes Fernández, Lorena", "Daza Carrasco, Mauricio",
  "Domínguez Donoso, Gaspar", "Fuchslocher Baeza, Javier", "Martin Bravo, Juan José", "Martínez Llancapan, Helmuth",
  "Namor Kong, Guillermo", "Politzer Kerekes, Patricia", "Pustilnick Arditi, Tammy", 
  "Sepúlveda Sepúlveda, Carolina", "Valenzuela Rio, Paulina"
)

# Movimientos Sociales Constituyentes
selected_names_MSC <- c(
  "Alvarado Jorquera, Gloria", "Carrillo Vidal, Alondra", "Dorador Ortiz, Cristina", "Flores Carlos, Alejandra",
  "Giustinianovich Campos, Elisa", "Hoppe Espoz, Vanessa", "Labbé Salazar, Bastián", "Meneses Palma, Janis",
  "Quinteros Cáceres, María Elisa", "Royo Letelier, Manuela", "Saldaña Muñoz, Alvin", "San Juan Standen, Constanza",
  "Vilches Fuenzalida, Carolina"
)

# Independientes-RN-Evópoli
selected_names_IND_RN_EV <- c(
  "Cozzi Elzo, Ruggero", 
  "De la Maza Bañados, Bernardo", 
  "Fontaine Talavera, Bernardo",
  "Labra Besserer, Patricia", 
  "Larraín Matte, Hernán", 
  "Mayol Bouchon, Luis",
  "Monckeberg Bruner, Cristian",
  "Ossandón Lira, Manuel",
  "Navarrete Arratia, Geoconda",
  "Rebolledo Aguirre, Bárbara",
  "Silva Mora, Luciano", 
  "Tepper Kolossa, María Angélica", 
  "Vega Campusano, Roberto", 
  "Veloso Muñoz, Paulina",
  "Celis Montt, Raúl",
  "Jofré Cáceres, Alvaro"
)

# Pueblo Constituyente
selected_names_PC <- c(
  "Arauna Urrutia, Francisca", "Bravo Silva, Daniel", "Caamaño Rojas, Francisco", 
  "González Araya, Dayyana", "Henríquez Carreño, Natalia", 
  "Salinas Manfredini, Fernando", "Uribe Araya, César", 
  "Vallejos Dávila, Loreto", "Villena Narbona, Ingrid", 
  "Zárate Zárate, Camila", "Rojas Vade, Rodrigo"
)

# Pueblos indígenas
selected_names_PI <- c(
  "Aguilera Hey, Tiare", "Catrileo Arias, Rosa", 
  "Galleguillos Aymani, Félix", 
  "González Calderón, Lidia", 
  "Jiménez Cáceres, Luis",
  "Loncon Antileo, Elisa",
  "Mamani Mamani, Isabella",
  "Millabur Ñancuil, Adolfo",
  "Tirado Soto, Fernando"
)

# Unidos por Chile
selected_names_UPC <- c(
  "Alvarez Zenteno, Rodrigo",
  "Arancibia Reyes, Jorge",
  "Cretton Rebolledo, Eduardo",
  "Hube Portus, Constanza",
  "Montealegre Navarro, Katerine",
  "Neumann Bertin, Ricardo",
  "Rivera Bigas, Pollyana",
  "Ubilla Pérez, María Cecilia"
)

# Colectivo del Apruebo
selected_names_CA <- c(
  "Barceló Amado, Luis",
  "Botto Salinas, Miguel Ángel",
  "Castillo Vigouroux, Eduardo",
  "Chahín Valenzuela, Fuad",
  "Harboe Bascuñán, Felipe",
  "Logan Soto, Rodrigo",
  "Squella Narducci, Agustín"
)

# Un Chile Unido
selected_names_UCU <- c(
  "Bown Sepúlveda, Carol",
  "Castro Gutiérrez, Claudia",
  "Cubillos Sigall, Marcela",
  "Mena Villar, Felipe",
  "Moreno Echeverría, Alfredo",
  "Toloza Fernández, Pablo",
  "Zúñiga Jory, Luis Arturo"
)

# Chile Libre
selected_names_CL <- c(
  "Arrau García-Huidobro, Martín",
  "Cantuarias Rubio, Rocío",
  "Hurtado Olave, Ruth",
  "Jurgensen Caesar, Harry",
  "Letelier Cortés, Margarita",
  "Marinovic Vial, Teresa"
)

# Somos Región [DEBUTA EL 17-03-2022, HASTA ENTONCES ELLOS ERAN DE RN-EVO:
#"Silva Mora, Luciano", 
#"Tepper Kolossa, María Angélica", 
#"Vega Campusano, Roberto", 
#"Veloso Muñoz, Paulina",
#"Celis Montt, Raúl",
#"Jofré Cáceres, Álvaro"
# selected_names_SR <- c(
#   "Celis Montt, Raúl", 
#   "Jofré Cáceres, Alvaro", 
#   "Silva Mora, Luciano", 
#   "Tepper Kolossa, María Angélica", 
#   "Vega Campusano, Roberto", 
#   "Veloso Muñoz, Paulina"
# )

# Mixto (varios)
selected_names_MIX <- c(
  "Ampuero Barrientos, Adriana", "Andrade León, Cristóbal", "Vidal Hernández, Loreto"
)

# Lista del Apruebo
selected_names_LA <- c(
  "Garín González, Renato"
)

total_names <- length(selected_names_FA) + 
  length(selected_names_CHD) + 
  length(selected_names_CCPP) + 
  length(selected_names_CS) + 
  length(selected_names_INC) + 
  length(selected_names_MSC) + 
  length(selected_names_IND_RN_EV) + 
  length(selected_names_PC) + 
  length(selected_names_PI) + 
  length(selected_names_UPC) + 
  length(selected_names_CA) + 
  length(selected_names_UCU) + 
  length(selected_names_CL) + 
  #length(selected_names_SR) + 
  length(selected_names_MIX) + 
  length(selected_names_LA)


# Aplicar transformación a todos los conjuntos de nombres
# selected_names_FA <- sapply(selected_names_FA, transform_votaciones_names)
# selected_names_CHD <- sapply(selected_names_CHD, transform_votaciones_names)
# selected_names_CCPP <- sapply(selected_names_CCPP, transform_votaciones_names)
# selected_names_CS <- sapply(selected_names_CS, transform_votaciones_names)
# selected_names_INC <- sapply(selected_names_INC, transform_votaciones_names)
# selected_names_MSC <- sapply(selected_names_MSC, transform_votaciones_names)
# selected_names_IND_RN_EV <- sapply(selected_names_IND_RN_EV, transform_votaciones_names)
# selected_names_PC <- sapply(selected_names_PC, transform_votaciones_names)
# selected_names_PI <- sapply(selected_names_PI, transform_votaciones_names)
# selected_names_UPC <- sapply(selected_names_UPC, transform_votaciones_names)
# selected_names_CA <- sapply(selected_names_CA, transform_votaciones_names)
# selected_names_UCU <- sapply(selected_names_UCU, transform_votaciones_names)
# selected_names_CL <- sapply(selected_names_CL, transform_votaciones_names)
# selected_names_SR <- sapply(selected_names_SR, transform_votaciones_names)
# selected_names_MIX <- sapply(selected_names_MIX, transform_votaciones_names)
# selected_names_LA <- sapply(selected_names_LA, transform_votaciones_names)

# # Load necessary libraries
# library(stringi)
# 
# # Function to remove accents from a string
# remove_accents <- function(text) {
#   stri_trans_general(text, "Latin-ASCII")
# }
# 
# # Apply the function to remove accents from NOMBRE in votaciones_df
# votaciones_df$NOMBRE <- sapply(votaciones_df$NOMBRE, remove_accents)
# 
# # Apply the function to remove accents from all coalition lists
# selected_names_FA <- sapply(selected_names_FA, remove_accents)
# selected_names_CHD <- sapply(selected_names_CHD, remove_accents)
# selected_names_CCPP <- sapply(selected_names_CCPP, remove_accents)
# selected_names_CS <- sapply(selected_names_CS, remove_accents)
# selected_names_INC <- sapply(selected_names_INC, remove_accents)
# selected_names_MSC <- sapply(selected_names_MSC, remove_accents)
# selected_names_IND_RN_EV <- sapply(selected_names_IND_RN_EV, remove_accents)
# selected_names_PC <- sapply(selected_names_PC, remove_accents)
# selected_names_PI <- sapply(selected_names_PI, remove_accents)
# selected_names_UPC <- sapply(selected_names_UPC, remove_accents)
# selected_names_CA <- sapply(selected_names_CA, remove_accents)
# selected_names_UCU <- sapply(selected_names_UCU, remove_accents)
# selected_names_CL <- sapply(selected_names_CL, remove_accents)
# selected_names_SR <- sapply(selected_names_SR, remove_accents)
# selected_names_MIX <- sapply(selected_names_MIX, remove_accents)
# selected_names_LA <- sapply(selected_names_LA, remove_accents)

# Function to assign coalition
assign_coalition <- function(name) {
  if (name %in% selected_names_FA) { 
    return("FA") # Frente Amplio
  } else if (name %in% selected_names_CHD) { 
    return("CHD") # Chile Digno
  } else if (name %in% selected_names_CCPP) { 
    return("CCPP") # Coordinadora Constituyente Plurinacional y Popular
  } else if (name %in% selected_names_CS) { 
    return("CS") # Colectivo Socialista
  } else if (name %in% selected_names_INC) { 
    return("INC") # Independientes por una Nueva Constitución
  } else if (name %in% selected_names_MSC) { 
    return("MSC") # Movimientos Sociales Constituyentes
  } else if (name %in% selected_names_IND_RN_EV) { 
    return("IND-RN-EV") # Independientes-RN-Evópoli
  } else if (name %in% selected_names_PC) { 
    return("PC") # Pueblo Constituyente
  } else if (name %in% selected_names_PI) { 
    return("PI") # Pueblos Indígenas
  } else if (name %in% selected_names_UPC) { 
    return("UPC") # Unidos por Chile
  } else if (name %in% selected_names_CA) { 
    return("CA") # Colectivo del Apruebo
  } else if (name %in% selected_names_UCU) { 
    return("UCU") # Un Chile Unido
  } else if (name %in% selected_names_CL) { 
    return("CL") # Chile Libre
  } else if (name %in% selected_names_MIX) { 
    return("MIX") # Mixto (varios)
  } else if (name %in% selected_names_LA) { 
    return("LA") # Lista del Apruebo
  } else {
    return("Other") # Otro / No clasificado
  }
}
#else if (name %in% selected_names_SR) { 
#return("SR") # Somos Región} 

# Adding new column to COALICION
votaciones_df <- votaciones_df %>%
  mutate(COALICION = sapply(NOMBRE, assign_coalition))

nombres_a_modificar_en_selected_names <- votaciones_df$NOMBRE[votaciones_df$COALICION=='Other'][1:5]
nombres_a_modificar_en_selected_names
nombres_en_base_datos <- votaciones_df$NOMBRE[1:155]
nombres_en_base_datos

# TODO OK !!

# -----------------------------------------------------------------------------

# Paso 1: Crear un diccionario de nombres completos
diccionario_coaliciones <- c(
  "FA" = "Frente Amplio",
  "CHD" = "Chile Digno",
  "CCPP" = "Coordinadora Constituyente Plurinacional y Popular",
  "CS" = "Colectivo Socialista",
  "INC" = "Independientes por una Nueva Constitución",
  "MSC" = "Movimientos Sociales Constituyentes",
  "IND-RN-EV" = "Independientes-RN-Evópoli",
  "PC" = "Pueblo Constituyente",
  "PI" = "Pueblos Indígenas",
  "UPC" = "Unidos por Chile",
  "CA" = "Colectivo del Apruebo",
  "UCU" = "Un Chile Unido",
  "CL" = "Chile Libre",
  "SR" = "Somos Región",
  "MIX" = "Mixto (varios)",
  "LA" = "Lista del Apruebo",
  "Other" = "Otro / No clasificado"
)

# Paso 2: Crear la nueva columna con nombres completos
votaciones_df <- votaciones_df %>%
  mutate(COALICION_COMPLETA = diccionario_coaliciones[COALICION])

# Paso 3: Generar el plot con los nombres completos
ggplot(votaciones_df, aes(x = factor(VOTACIONID), y = NOMBRE, fill = factor(VOTACION))) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("-1" = "red", "0" = "yellow", "1" = "green", "9" = "gray"),
    name = "Votación",
    labels = c("En Contra (-1)", "Abstención (0)", "A Favor (1)", "No Vota (9)")
  ) +
  facet_wrap(~COALICION_COMPLETA, scales = "free_y", ncol = 1) +  # Ahora usa nombres completos
  labs(
    title = paste("Heatmap de Votaciones Sesión", votaciones_df$FECHA[1]),
    x = "VOTACIONID",
    y = "Nombre"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
    strip.text = element_text(face = "bold", size = 10)  # Se verá mejor el nombre de cada coalición
  )
