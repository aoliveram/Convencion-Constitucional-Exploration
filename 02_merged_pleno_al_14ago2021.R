# Tengo que ver las votaciones de otras fechas. 
# Crear un solo dataset con las votaciones hasta __ / __ / 2022

# ¿Cuántas sesiónes están contenidas en RCP_votaciones_al_14ago2021.csv ?
# "En el período bajo análisis se registraron en total 15 sesiones plenarias."
# Sesiones VACÍAS: 1-2-3-4-6-9-11
# Sesiones RARAS: 5 (PCR) - NO considerar
# 
# Sí considerar: 7-8-10-12-13-14-15

library(readxl)
library(dplyr)

sesion_7 <- read_excel('Pleno/sesion_7.xls', col_names = FALSE)
# sesion_8 <- read_excel('Pleno/sesion_8.xls', col_names = FALSE)
# sesion_10 <- read_excel('Pleno/sesion_10.xls', col_names = FALSE)
# sesion_12 <- read_excel('Pleno/sesion_12.xls', col_names = FALSE)
# sesion_13 <- read_excel('Pleno/sesion_13.xls', col_names = FALSE)
# sesion_14 <- read_excel('Pleno/sesion_14.xls', col_names = FALSE)
# sesion_15 <- read_excel('Pleno/sesion_15.xls', col_names = FALSE)

# Identify rows where the first column contains "VOTACIONID"
start_indices <- which(sesion_7[[1]] == "VOTACIONID")

# Create a list to store blocks of data
blocks <- list()

for (i in seq_along(start_indices)) {
  start_idx <- start_indices[i]
  end_idx <- ifelse(i < length(start_indices), start_indices[i + 1] - 1, nrow(sesion_7))
  
  # Extract the block and store it in the list
  block <- sesion_7[start_idx:end_idx, ]
  blocks[[i]] <- block
}

# Constructing a single data frame
library(dplyr)
votaciones_sesion_7 <- list()

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
  votaciones_sesion_7[[i]] <- temp_data
}

# Combine all blocks into a single data frame
votaciones_sesion_7_df <- as.data.frame(do.call(rbind, votaciones_sesion_7))

# Delete FECHA
votaciones_sesion_7_df <- votaciones_sesion_7_df[, c("NOMBRE", "VOTACION", "VOTACIONID")]

# Creating more columns
library(tidyr)
votaciones_sesion_7_df <- votaciones_sesion_7_df %>%
  spread(VOTACIONID, VOTACION)

# Saving
write.csv(votaciones_sesion_7_df, 
          file = "Pleno/votaciones_sesion_7_df.csv", 
          row.names = FALSE)


# ------------------- NEW

library(readxl)
library(dplyr)
library(tidyr)

# Funcioón para separar votaciones dentro de una sesión
process_session <- function(file_path) {
  session_data <- read_xls(file_path, col_names = FALSE)
  
  # Identificamos "VOTACIONID"
  start_indices <- which(session_data[[1]] == "VOTACIONID")
  
  # Creamos una lista para el conjunto de votaciones
  blocks <- list()
  
  for (i in seq_along(start_indices)) {
    start_idx <- start_indices[i]
    end_idx <- ifelse(i < length(start_indices), start_indices[i + 1] - 1, nrow(session_data))
    
    # Añadimos votación a la lista
    block <- session_data[start_idx:end_idx, ]
    blocks[[i]] <- block
  }
  
  # Procesamos la sesión
  votaciones_session <- list()
  
  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    
    votacion_id <- block[2, 1][[1]]
    fecha_raw <- block[2, 3][[1]]
    fecha <- format(as.Date(substr(fecha_raw, 1, 10), format = "%d-%m-%Y"), "%m-%d")
    
    voting_data <- block[-c(1:3), ]
    colnames(voting_data) <- c("NA1","NOMBRE", "VOTACION", "NA2", "NA3", "NA4", "NA5", "NA6", "NA7")
    
    temp_data <- voting_data %>%
      select(NOMBRE, VOTACION) %>%
      mutate(
        VOTACIONID = paste(fecha, votacion_id, sep="_"),  # Incluir FECHA en VOTACIONID
        VOTACION = case_when(
          VOTACION == "AFIRMATIVO" ~ 1,
          VOTACION == "ABSTENCION" ~ 0,
          VOTACION == "EN CONTRA" ~ -1,
          VOTACION == "NO VOTA" ~ 9,
          TRUE ~ NA_real_
        )
      )
    
    votaciones_session[[i]] <- temp_data
  }
  
  # Añadimos la info procesada de la sesión a votaciones_session_df
  votaciones_session_df <- as.data.frame(do.call(rbind, votaciones_session))
  
  return(votaciones_session_df)
}

# Lista de sesiones
session_files <- c('Pleno/sesion_7.xls', 'Pleno/sesion_8.xls', 'Pleno/sesion_10.xls', 
                   'Pleno/sesion_12.xls', 'Pleno/sesion_13.xls', 'Pleno/sesion_14.xls', 
                   'Pleno/sesion_15.xls')

# Procesamos todas las sesiones
all_sessions <- lapply(session_files, process_session)

# Combinamos las sesiones en un data frame
all_votaciones_df <- do.call(rbind, all_sessions)

# Creamos una columna por cada votación 
final_votaciones_df <- all_votaciones_df %>%
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

# Guardamos objeto
write.csv(final_votaciones_df, 
          file = "Pleno/votaciones_al_14ago2021_manual.csv", 
          row.names = FALSE)

votaciones_al_14ago2021_manual <-read.csv("Pleno/votaciones_al_14ago2021_manual.csv") # 146 votaciones

# LISTO, YA TENGO TODAS LAS VOTACIONES DE votaciones_al_14ago2021.csv !!!!!
