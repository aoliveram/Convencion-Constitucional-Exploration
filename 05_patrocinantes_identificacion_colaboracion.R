# --- ANÁLISIS DE COLABORADORES ÚNICOS POR CONVENCIONAL ---

library(jsonlite)
library(dplyr)
library(purrr)
library(ggplot2)

# --- 1. Carga y Consolidación de Datos ---
# (Este bloque es el mismo que usamos antes para cargar todos los datos)
data_folder <- "patrocinantes_identificacion"
json_files <- list.files(path = data_folder,
                         pattern = "api_extracted_\\d+_\\d+_corrected_3\\.json$",
                         full.names = TRUE)

if (length(json_files) == 0) {
  stop("No se encontraron archivos JSON en la carpeta '", data_folder, "'.")
}

process_json_file <- function(file_path) {
  raw_data <- fromJSON(file_path)
  imap_dfr(raw_data, ~{
    doc_id_match <- regexpr("^[0-9]+", .y)
    if (doc_id_match == -1) return(NULL)
    doc_id <- regmatches(.y, doc_id_match)
    firmantes <- .x$firmantes_matched
    if (is.null(firmantes) || length(firmantes) == 0) return(NULL)
    tibble(doc_id = doc_id, convencional = firmantes)
  })
}

all_initiatives_df <- map_dfr(json_files, process_json_file)
cat(paste("Total de patrocinios individuales procesados:", nrow(all_initiatives_df), "\n"))


# --- 2. Cálculo de Colaboradores Únicos ---
cat("Calculando el número de colaboradores únicos por convencional...\n")

# Paso A: Realizar un "self-join" para encontrar todos los pares de co-firmantes.
# Unimos la tabla consigo misma usando el 'doc_id' como clave.
co_sponsorships <- inner_join(
  all_initiatives_df,
  all_initiatives_df,
  by = "doc_id",
  suffix = c("_A", "_B") # Nombres para las columnas duplicadas
)

# Paso B: Filtrar para quedarse solo con los pares de co-firmantes distintos.
# Se elimina el caso donde un convencional aparece con sí mismo.
co_sponsorships_filtered <- co_sponsorships %>%
  filter(convencional_A != convencional_B)

# Paso C: Agrupar por cada convencional y contar sus colaboradores únicos.
# n_distinct() es la función clave que cuenta los valores únicos.
co_sponsor_counts <- co_sponsorships_filtered %>%
  group_by(convencional_A) %>%
  summarise(
    unique_co_sponsors = n_distinct(convencional_B)
  ) %>%
  rename(convencional = convencional_A) # Renombrar para mayor claridad

# Paso D: Asegurar que los 154 convencionales estén presentes, incluso si no colaboraron.
# Primero, obtenemos la lista completa de todos los convencionales.
all_conventionals_df <- all_initiatives_df %>%
  distinct(convencional)

# Luego, hacemos un left_join para incluir a todos. Los que no tengan co-firmas quedarán con NA.
final_counts <- all_conventionals_df %>%
  left_join(co_sponsor_counts, by = "convencional") %>%
  mutate(
    # Reemplazamos los NA por 0.
    unique_co_sponsors = tidyr::replace_na(unique_co_sponsors, 0)
  )

cat("Cálculo finalizado.\n")


# --- 3. Generación del Gráfico ---
cat("Generando el gráfico...\n")

ggplot(final_counts, 
       # aes() define la estética: ejes x e y.
       # reorder() ordena el eje x (los nombres) de forma descendente (-) según el recuento.
       aes(x = reorder(convencional, -unique_co_sponsors), y = unique_co_sponsors)) +
  
  # geom_col() crea el gráfico de barras.
  geom_col(fill = "steelblue", width = 0.7) +
  
  # labs() define los títulos y etiquetas de los ejes.
  labs(
    title = "Alcance de Colaboración por Convencional",
    subtitle = "Número de co-firmantes únicos con los que ha patrocinado al menos una iniciativa",
    x = "Convencional",
    y = "Número de Colaboradores Únicos"
  ) +
  
  # theme() ajusta la apariencia del gráfico.
  theme_minimal(base_size = 12) + # Un tema limpio
  theme(
    # Rota las etiquetas del eje x para que sean legibles.
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    # Añade una grilla en el eje y para facilitar la lectura de los valores.
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15))
  )

cat("Gráfico generado. Revísalo en la ventana de Plots.\n")