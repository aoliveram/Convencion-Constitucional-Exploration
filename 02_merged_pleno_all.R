# En el primer data frame (al 14ago2021), se contemplan 39 días. 
# Tomaré las sesiones separadas por 39 días entre ellas

library(readxl)
library(dplyr)
library(tidyr)
library(readr)

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
    
    
    votacion_id <- sprintf("%02d", as.numeric(block[2, 1][[1]]))  # Zero-pad votacion_id
    fecha_raw <- block[2, 3][[1]]
    fecha <- format(as.Date(substr(fecha_raw, 1, 10), format = "%d-%m-%Y"), "%d%m%Y")  # Format as DDMMYYYY
    
    voting_data <- block[-c(1:3), ]
    colnames(voting_data) <- c("NA1","NOMBRE", "VOTACION", "NA2", "NA3", "NA4", "NA5", "NA6", "NA7")
    
    temp_data <- voting_data %>%
      select(NOMBRE, VOTACION) %>%
      mutate(
        VOTACIONID = paste0("X", fecha, "_", votacion_id),  # Generate column name in desired format
        VOTACION = case_when(
          VOTACION == "AFIRMATIVO" ~ 1,
          VOTACION == "ABSTENCION" ~ NA,
          VOTACION == "EN CONTRA" ~ 0,
          VOTACION == "NO VOTA" ~ NA,
          TRUE ~ NA
        )
      )
    
    votaciones_session[[i]] <- temp_data
  }
  
  # Añadimos la info procesada de la sesión a votaciones_session_df
  votaciones_session_df <- as.data.frame(do.call(rbind, votaciones_session))
  
  return(votaciones_session_df)
}

# ------------------------------- 01-15

session_files <- c('data - pleno/sesion_5.xls', 'data - pleno/sesion_7.xls', 'data - pleno/sesion_8.xls', 
                   'data - pleno/sesion_10.xls', 'data - pleno/sesion_12.xls', 'data - pleno/sesion_13.xls', 
                   'data - pleno/sesion_14.xls', 'data - pleno/sesion_15.xls')

all_sessions <- lapply(session_files, process_session)
all_votaciones_df <- do.call(rbind, all_sessions)

final_votaciones_df <- all_votaciones_df %>%
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

final_votaciones_df <- final_votaciones_df[-nrow(final_votaciones_df), ]

write.csv(final_votaciones_df, 
          file = "ideological-scaling-files/votaciones_01_15.csv", 
          row.names = FALSE)

# votaciones_al_14ago2021_manual_2 <-read.csv("ideological-scaling-files/votaciones_al_14ago2021_manual_2.csv") # __ votaciones
# votaciones_al_14ago2021_manual_2$X14072021_880[155] # Arturo Zúñiga NA
# 
# votaciones_Luis_Zuniga_1 <- votaciones_al_14ago2021_manual_2 %>% filter(NOMBRE == "Zúñiga Jory, Luis Arturo")
# votaciones_Luis_Zuniga_1 <- votaciones_Luis_Zuniga_1[,-1]
# table(unlist(votaciones_Luis_Zuniga_1), useNA = "ifany")
# 
# votaciones_al_14ago2021 <- read_csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv", locale = locale(encoding = "LATIN1"))
# votaciones_al_14ago2021$X14072021_05[155] # Arturo Zúniga 0
# votaciones_Luis_Zuniga_2 <- votaciones_al_14ago2021 %>% filter(candidato == "Zúñiga, Luis Arturo")
# votaciones_Luis_Zuniga_2 <- votaciones_Luis_Zuniga_2[,-c(1,2)]
# table(unlist(votaciones_Luis_Zuniga_2), useNA = "ifany")

# NO son equivalentes la mía y la de Jorge. 
# Sin embargo, sirve para comparar ventanas de tiempo.

votaciones_01_15 <-read.csv("ideological-scaling-files/votaciones_01_15.csv") # 147 votaciones

# ----------------------------- 16 - 21

session_files <- c('data - pleno/sesion_16.xls', 'data - pleno/sesion_17.xls', 'data - pleno/sesion_18.xls', 
                   'data - pleno/sesion_20.xls', 'data - pleno/sesion_21.xls')
all_sessions <- lapply(session_files, process_session)
all_votaciones_df <- do.call(rbind, all_sessions)

final_votaciones_df <- all_votaciones_df %>%
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

write.csv(final_votaciones_df, 
          file = "ideological-scaling-files/votaciones_16_21.csv", 
          row.names = FALSE)

votaciones_16_21 <-read.csv("ideological-scaling-files/votaciones_16_21.csv") # 182 votaciones

# ----------------------------- 22 - 37

# Sesiones VACÍAS: 31 - 32 - 33 - 34 - 35

session_files <- c('data - pleno/sesion_22.xls', 'data - pleno/sesion_23.xls', 'data - pleno/sesion_24.xls', 
                   'data - pleno/sesion_25.xls', 'data - pleno/sesion_26.xls', 'data - pleno/sesion_27.xls',
                   'data - pleno/sesion_28.xls', 'data - pleno/sesion_29.xls', 'data - pleno/sesion_30.xls',
                   'data - pleno/sesion_36.xls', 'data - pleno/sesion_37.xls')

all_sessions <- lapply(session_files, process_session)
all_votaciones_df <- do.call(rbind, all_sessions)

final_votaciones_df <- all_votaciones_df %>%
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

final_votaciones_df <- final_votaciones_df[-nrow(final_votaciones_df), ]

write.csv(final_votaciones_df, 
          file = "ideological-scaling-files/votaciones_22_37.csv", 
          row.names = FALSE)

votaciones_22_37 <-read.csv("ideological-scaling-files/votaciones_22_37.csv") # 599 votaciones

# ----------------------------- 38 - 46
# 49 días
# Sesiones VACÍAS: 38 - 40 - 41

session_files <- c('data - pleno/sesion_39.xls', 'data - pleno/sesion_42.xls', 'data - pleno/sesion_43.xls', 
                   'data - pleno/sesion_44.xls', 'data - pleno/sesion_45.xls', 'data - pleno/sesion_46.xls')

all_sessions <- lapply(session_files, process_session)
all_votaciones_df <- do.call(rbind, all_sessions)

final_votaciones_df <- all_votaciones_df %>%
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

final_votaciones_df <- final_votaciones_df[-150,]

write.csv(final_votaciones_df, 
          file = "ideological-scaling-files/votaciones_38_46.csv", 
          row.names = FALSE)

votaciones_38_46 <-read.csv("ideological-scaling-files/votaciones_38_46.csv") # 52 votaciones

# ----------------------------- 47 - 55
# 42 días
# Sesiones VACÍAS: 47

session_files <- c('data - pleno/sesion_48.xls', 'data - pleno/sesion_49.xls', 'data - pleno/sesion_50.xls', 
                   'data - pleno/sesion_51.xls', 'data - pleno/sesion_52.xls', 'data - pleno/sesion_53.xls',
                   'data - pleno/sesion_54.xls', 'data - pleno/sesion_55.xls')

all_sessions <- lapply(session_files, process_session)
all_votaciones_df <- do.call(rbind, all_sessions)

final_votaciones_df <- all_votaciones_df %>%
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

final_votaciones_df <- final_votaciones_df[-c(nrow(final_votaciones_df), nrow(final_votaciones_df)-5), ] # eliminamos *failed to decode

write.csv(final_votaciones_df, 
          file = "ideological-scaling-files/votaciones_47_55.csv", 
          row.names = FALSE)

votaciones_47_55 <-read.csv("ideological-scaling-files/votaciones_47_55.csv") # 65 votaciones

# ----------------------------- 56 - 75
# 37 días
# Sesiones VACÍAS: 

session_files <- c('data - pleno/sesion_56.xls', 'data - pleno/sesion_57.xls', 'data - pleno/sesion_58.xls', 
                   'data - pleno/sesion_59.xls', 'data - pleno/sesion_60.xls', 'data - pleno/sesion_61.xls',
                   'data - pleno/sesion_62.xls', 'data - pleno/sesion_63.xls', 'data - pleno/sesion_64.xls',
                   'data - pleno/sesion_65.xls', 'data - pleno/sesion_66.xls', 'data - pleno/sesion_67.xls',
                   'data - pleno/sesion_68.xls', 'data - pleno/sesion_69.xls', 'data - pleno/sesion_70.xls',
                   'data - pleno/sesion_71.xls', 'data - pleno/sesion_72.xls', 'data - pleno/sesion_73.xls',
                   'data - pleno/sesion_74.xls', 'data - pleno/sesion_75.xls')

all_sessions <- lapply(session_files, process_session)
all_votaciones_df <- do.call(rbind, all_sessions)

final_votaciones_df <- all_votaciones_df %>%
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

write.csv(final_votaciones_df, 
          file = "ideological-scaling-files/votaciones_56_75.csv", 
          row.names = FALSE)

votaciones_56_75 <-read.csv("ideological-scaling-files/votaciones_56_75.csv") # 900 votaciones

# ----------------------------- 76 - 99
# 39 días
# Sesiones VACÍAS: 

session_files <- c('data - pleno/sesion_76.xls', 'data - pleno/sesion_77.xls', 'data - pleno/sesion_78.xls', 
                   'data - pleno/sesion_79.xls', 'data - pleno/sesion_80.xls', 'data - pleno/sesion_81.xls',
                   'data - pleno/sesion_82.xls', 'data - pleno/sesion_83.xls', 'data - pleno/sesion_84.xls',
                   'data - pleno/sesion_85.xls', 'data - pleno/sesion_86.xls', 'data - pleno/sesion_87.xls',
                   'data - pleno/sesion_88.xls', 'data - pleno/sesion_89.xls', 'data - pleno/sesion_90.xls',
                   'data - pleno/sesion_91.xls', 'data - pleno/sesion_92.xls', 'data - pleno/sesion_93.xls',
                   'data - pleno/sesion_94.xls', 'data - pleno/sesion_95.xls', 'data - pleno/sesion_96.xls',
                   'data - pleno/sesion_97.xls', 'data - pleno/sesion_98.xls', 'data - pleno/sesion_99.xls')

all_sessions <- lapply(session_files, process_session)
all_votaciones_df <- do.call(rbind, all_sessions)

final_votaciones_df <- all_votaciones_df %>%
  dplyr::distinct(NOMBRE, VOTACIONID, .keep_all = TRUE) %>%  # Elimina duplicados
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

#all_votaciones_df |>
#  dplyr::summarise(n = dplyr::n(), .by = c(NOMBRE, VOTACIONID)) |>
#  dplyr::filter(n > 1L)

write.csv(final_votaciones_df, 
          file = "ideological-scaling-files/votaciones_76_99.csv", 
          row.names = FALSE)

votaciones_76_99 <-read.csv("ideological-scaling-files/votaciones_76_99.csv") # 2183 votaciones

# ----------------------------- 100 - 106
# 39 días
# Sesiones VACÍAS: 104 - 105

session_files <- c('data - pleno/sesion_100.xls', 'data - pleno/sesion_101.xls', 'data - pleno/sesion_102.xls', 
                   'data - pleno/sesion_103.xls', 'data - pleno/sesion_106.xls')

all_sessions <- lapply(session_files, process_session)
all_votaciones_df <- do.call(rbind, all_sessions)

final_votaciones_df <- all_votaciones_df %>%
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

final_votaciones_df <- final_votaciones_df[-156,]

write.csv(final_votaciones_df, 
          file = "ideological-scaling-files/votaciones_100_106.csv", 
          row.names = FALSE)

votaciones_100_106 <-read.csv("ideological-scaling-files/votaciones_100_106.csv") # 515 votaciones

# ----------------------------- 107 - 109
# 10 días
# Sesiones VACÍAS: 104 - 105

session_files <- c('data - pleno/sesion_107.xls', 'data - pleno/sesion_108.xls', 'data - pleno/sesion_109.xls')

all_sessions <- lapply(session_files, process_session)
all_votaciones_df <- do.call(rbind, all_sessions)

final_votaciones_df <- all_votaciones_df %>%
  pivot_wider(names_from = VOTACIONID, values_from = VOTACION)

write.csv(final_votaciones_df, 
          file = "ideological-scaling-files/votaciones_107_109.csv", 
          row.names = FALSE)

votaciones_107_109 <-read.csv("ideological-scaling-files/votaciones_107_109.csv") # 72 votaciones
