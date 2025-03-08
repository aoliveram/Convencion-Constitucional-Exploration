
# En el primer data frame (al 14ago2021), se contemplan 39 días. 
# Tomaré las sesiones separadas por 39 días entre ellas

# Sesiones VACÍAS: 19


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
session_files <- c('Pleno/sesion_16.xls', 'Pleno/sesion_17.xls', 'Pleno/sesion_18.xls', 
                   'Pleno/sesion_20.xls', 'Pleno/sesion_21.xls')

# Procesamos todas las sesiones
all_sessions <- lapply(session_files, process_session)

# Combinamos las sesiones en un data frame
all_votaciones_df <- do.call(rbind, all_sessions)

# Creamos una columna por cada votación
final_votaciones_df <- all_votaciones_df %>%
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

# Guardamos objeto
write.csv(final_votaciones_df, 
          file = "Pleno/votaciones_16_21.csv", 
          row.names = FALSE)

votaciones_16_21 <-read.csv("Pleno/votaciones_16_21.csv") # 183 votaciones

# ----------------------------- RESTO

# 22 - 37
# Sesiones VACÍAS: 31 - 32 - 33 - 34 - 35

# Lista de sesiones
session_files <- c('Pleno/sesion_22.xls', 'Pleno/sesion_23.xls', 'Pleno/sesion_24.xls', 
                   'Pleno/sesion_25.xls', 'Pleno/sesion_26.xls', 'Pleno/sesion_27.xls',
                   'Pleno/sesion_28.xls', 'Pleno/sesion_29.xls', 'Pleno/sesion_30.xls',
                   'Pleno/sesion_36.xls', 'Pleno/sesion_37.xls')

all_sessions <- lapply(session_files, process_session)
all_votaciones_df <- do.call(rbind, all_sessions)

final_votaciones_df <- all_votaciones_df %>%
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

write.csv(final_votaciones_df, 
          file = "Pleno/votaciones_22_37.csv", 
          row.names = FALSE)

votaciones_22_37 <-read.csv("Pleno/votaciones_22_37.csv") # 599 votaciones

votaciones_al_14ago2021 <- read_csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv", locale = locale(encoding = "LATIN1"))
votaciones_al_14ago2021_manual <-read.csv("Pleno/votaciones_al_14ago2021_manual.csv") # 146 votaciones

