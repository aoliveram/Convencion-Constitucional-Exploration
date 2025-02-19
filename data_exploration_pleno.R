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
votaciones_df$NOMBRE <- sapply(votaciones_df$NOMBRE, transform_votaciones_names)

# Frente Amplio
selected_names_FA <- c(
  "Damaris Abarca", "Ignacio Achurra", "Amaya Alvez", "Fernando Atria", 
  "Jorge Baradit", "Jaime Bassa", "Aurora Delgado", "Yarela Gómez", 
  "Jeniffer Mella", "María José Oyarzún", "Giovanna Roa", "Beatriz Sánchez", 
  "Constanza Schönhaut", "Mariela Serey", "Daniel Stingo", "Tatiana Urrutia", 
  "Christian Viera"
)

# Chile Digno
selected_names_CHD <- c(
  "Marcos Barraza", "Roberto Celedón", "Bessy Gallardo", "Paola Grandón",
  "Hugo Gutiérrez", "Valentina Miranda", "Nicolás Núñez", "Ericka Portilla",
  "Bárbara Sepúlveda", "Hernán Velásquez", "Carolina Videla"
)

# Coordinadora Constituyente Plurinacional y Popular
selected_names_CCPP <- c(
  "Victorino Antilef", "Marco Arellano", "Wilfredo Bacian", "Alexis Caiguan",
  "Eric Chinga", "Isabel Godoy", "Giovanna Grandón", "Elsa Labraña",
  "Francisca Linconao", "Natividad Llanquileo", "Tania Madriaga",
  "Ivanna Olivares", "Alejandra Pérez", "María Rivera", "Margarita Vargas",
  "Lisette Vergara", "Manuel Woldarsky"
)

# Colectivo Socialista
selected_names_CS <- c(
  "Julio Álvarez", "Carlos Calvo", "Adriana Cancino", "María Trinidad Castillo",
  "Andrés Cruz", "Patricio Fernández", "Claudio Gómez", "Maximiliano Hurtado",
  "Tomás Laibe", "Ricardo Montero", "Pedro Muñoz", "Matías Orellana",
  "Malucha Pinto", "Ramona Reyes", "César Valenzuela", "Mario Vargas"
)

# Independientes por una Nueva Constitución
selected_names_INC <- c(
  "Jorge Abarca", "Benito Baranda", "Lorena Céspedes", "Mauricio Daza",
  "Gaspar Domínguez", "Javier Fuchslocher", "Juan José Martin", "Helmuth Martínez",
  "Guillermo Namor", "Patricia Politzer", "Tammy Pustilnick", 
  "Carolina Sepúlveda", "Paulina Valenzuela"
)

# Movimientos Sociales Constituyentes
selected_names_MSC <- c(
  "Gloria Alvarado", "Alondra Carrillo", "Cristina Dorador", "Alejandra Flores",
  "Elisa Giustinianovich", "Vanessa Hoppe", "Bastián Labbé", "Janis Meneses",
  "María Elisa Quinteros", "Manuela Royo", "Alvin Saldaña", "Constanza San Juan",
  "Carolina Vilches"
)

# Independientes-RN-Evópoli
selected_names_IND_RN_EV <- c(
  "Raúl Celis", "Ruggero Cozzi", "Bernardo de la Maza", "Bernardo Fontaine",
  "Álvaro Jofré", "Patricia Labra", "Hernán Larraín Matte", "Luis Mayol",
  "Cristián Monckeberg", "Geoconda Navarrete", "Manuel Ossandón",
  "Bárbara Rebolledo", "Luciano Silva", "Angélica Tepper", "Roberto Vega",
  "Paulina Veloso"
)

# Pueblo Constituyente
selected_names_PC <- c(
  "Francisca Arauna", "Daniel Bravo", "Francisco Caamaño", "Dayyana González",
  "Natalia Henríquez", "Fernando Salinas", "César Uribe", "Loreto Vallejos",
  "Ingrid Villena", "Camila Zárate"
)

# Pueblos indígenas
selected_names_PI <- c(
  "Tiare Aguilera", "Rosa Catrileo", "Félix Galleguillos", "Lidia González",
  "Luis Jiménez", "Elisa Loncon", "Isabella Mamani", "Adolfo Millabur",
  "Fernando Tirado"
)

# Unidos por Chile
selected_names_UPC <- c(
  "Rodrigo Álvarez", "Jorge Arancibia", "Eduardo Cretton", "Constanza Hube",
  "Katerine Montealegre", "Ricardo Neumann", "Pollyana Rivera",
  "María Cecilia Ubilla"
)

# Colectivo del Apruebo
selected_names_CA <- c(
  "Luis Barceló", "Miguel Ángel Botto", "Eduardo Castillo", "Fuad Chahín",
  "Felipe Harboe", "Rodrigo Logan", "Agustín Squella"
)

# Un Chile Unido
selected_names_UCU <- c(
  "Carol Bown", "Claudia Castro", "Marcela Cubillos", "Felipe Mena",
  "Alfredo Moreno", "Pablo Toloza", "Luis Zúñiga"
)

# Chile Libre
selected_names_CL <- c(
  "Martín Arrau", "Rocío Cantuarias", "Ruth Hurtado", "Harry Jürgensen",
  "Margarita Letelier", "Teresa Marinovic"
)

# Somos Región
selected_names_SR <- c(
  "Raúl Celis", "Álvaro Jofré", "Luciano Silva", "Angélica Tepper",
  "Roberto Vega", "Paulina Veloso"
)

# Mixto (varios)
selected_names_MIX <- c(
  "Adriana Ampuero", "Cristóbal Andrade", "Rossana Vidal"
)

# Lista del Apruebo
selected_names_LA <- c(
  "Renato Garín"
)


# Función para transformar el formato ("Apellido, Nombre")
transform_selected_names <- function(name) {
  parts <- unlist(strsplit(name, " "))
  first_name <- parts[1]
  surname <- paste(parts[-1], collapse = " ") # Para incluir apellidos compuestos
  return(paste(surname, ",", first_name))
}

# Aplicar transformación a todos los conjuntos de nombres
selected_names_FA <- sapply(selected_names_FA, transform_selected_names)
selected_names_CHD <- sapply(selected_names_CHD, transform_selected_names)
selected_names_CCPP <- sapply(selected_names_CCPP, transform_selected_names)
selected_names_CS <- sapply(selected_names_CS, transform_selected_names)
selected_names_INC <- sapply(selected_names_INC, transform_selected_names)
selected_names_MSC <- sapply(selected_names_MSC, transform_selected_names)
selected_names_IND_RN_EV <- sapply(selected_names_IND_RN_EV, transform_selected_names)
selected_names_PC <- sapply(selected_names_PC, transform_selected_names)
selected_names_PI <- sapply(selected_names_PI, transform_selected_names)
selected_names_UPC <- sapply(selected_names_UPC, transform_selected_names)
selected_names_CA <- sapply(selected_names_CA, transform_selected_names)
selected_names_UCU <- sapply(selected_names_UCU, transform_selected_names)
selected_names_CL <- sapply(selected_names_CL, transform_selected_names)
selected_names_SR <- sapply(selected_names_SR, transform_selected_names)
selected_names_MIX <- sapply(selected_names_MIX, transform_selected_names)
selected_names_LA <- sapply(selected_names_LA, transform_selected_names)

# Load necessary libraries
library(stringi)

# Function to remove accents from a string
remove_accents <- function(text) {
  stri_trans_general(text, "Latin-ASCII")
}

# Apply the function to remove accents from NOMBRE in votaciones_df
votaciones_df$NOMBRE <- sapply(votaciones_df$NOMBRE, remove_accents)

# Apply the function to remove accents from all coalition lists
selected_names_FA <- sapply(selected_names_FA, remove_accents)
selected_names_CHD <- sapply(selected_names_CHD, remove_accents)
selected_names_CCPP <- sapply(selected_names_CCPP, remove_accents)
selected_names_CS <- sapply(selected_names_CS, remove_accents)
selected_names_INC <- sapply(selected_names_INC, remove_accents)
selected_names_MSC <- sapply(selected_names_MSC, remove_accents)
selected_names_IND_RN_EV <- sapply(selected_names_IND_RN_EV, remove_accents)
selected_names_PC <- sapply(selected_names_PC, remove_accents)
selected_names_PI <- sapply(selected_names_PI, remove_accents)
selected_names_UPC <- sapply(selected_names_UPC, remove_accents)
selected_names_CA <- sapply(selected_names_CA, remove_accents)
selected_names_UCU <- sapply(selected_names_UCU, remove_accents)
selected_names_CL <- sapply(selected_names_CL, remove_accents)
selected_names_SR <- sapply(selected_names_SR, remove_accents)
selected_names_MIX <- sapply(selected_names_MIX, remove_accents)
selected_names_LA <- sapply(selected_names_LA, remove_accents)

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
  } else if (name %in% selected_names_SR) { 
    return("SR") # Somos Región
  } else if (name %in% selected_names_MIX) { 
    return("MIX") # Mixto (varios)
  } else if (name %in% selected_names_LA) { 
    return("LA") # Lista del Apruebo
  } else {
    return("Other") # Otro / No clasificado
  }
}

# Adding new column to COALICION
votaciones_df <- votaciones_df %>%
  mutate(COALICION = sapply(NOMBRE, assign_coalition))

head(votaciones_df)
str(votaciones_df)

# OJOOOOOOO


"Zuniga , Arturo" %in% unique(votaciones_df$NOMBRE) # --> FALSE
"Zuniga , Luis" %in% unique(votaciones_df$NOMBRE) # --> TRUE

# ASÍ QUE HAY QUE VER CUÁLES SON LOS NOMBRES REALES DE ESTOS COMPARES


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
