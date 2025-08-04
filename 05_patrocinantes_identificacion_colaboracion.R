# --- ANÁLISIS DE COLABORADORES ÚNICOS POR CONVENCIONAL ---

library(jsonlite)
library(dplyr)
library(purrr)
library(ggplot2)

# --- 1. Datos ---
data_folder <- "patrocinantes_identificacion"
json_files <- list.files(path = data_folder,
                         pattern = "api_extracted_\\d+_\\d+_corrected_3\\.json$",
                         full.names = TRUE)

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

# --- 2. Cálculo de Colaboradores Únicos ---

# A: Realizar un "self-join" para encontrar todos los pares de co-firmantes 
# usando el 'doc_id' como clave.
co_sponsorships <- inner_join(
  all_initiatives_df,
  all_initiatives_df,
  by = "doc_id",
  suffix = c("_A", "_B") # Nombres columnas duplicadas
)

# B: Filtrar co-firmantes iguales
co_sponsorships_filtered <- co_sponsorships %>%
  filter(convencional_A != convencional_B)

# C: Agrupar por cada convencional y contar valores unicos n_distinct().
co_sponsor_counts <- co_sponsorships_filtered %>%
  group_by(convencional_A) %>%
  summarise(
    unique_co_sponsors = n_distinct(convencional_B)
  ) %>%
  rename(convencional = convencional_A) # Renombrar para mayor claridad

# --- 3. Generación del Gráfico ---

pdf("scripts - plots/n_colaboracion_por_convencional.pdf", width = 10, height = 6)

ggplot(co_sponsor_counts, 
       aes(x = reorder(convencional, -unique_co_sponsors), y = unique_co_sponsors)) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(
    title = "N° de colaboradores por Convenciona",
    subtitle = "Número de co-patrocinadores únicos con los que ha patrocinado al menos una iniciativa",
    x = NULL,
    y = "N° de Colaboradores Únicos"
  ) +
  theme_minimal(base_size = 12) + # Un tema limpio
  theme(
    # Rota las etiquetas del eje x.
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    # grilla en el eje y.
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15))
  )

dev.off()

# --- 4. Generación del Gráfico Ordenado por Posición Política ---

library(readr) 
ordenamiento_1D_WNOM_01_15 <- read_csv("scripts - files/ordenamientos_pleno/ordenamiento_1D_WNOM_01-15.csv")

# Unimos usando 'convencional' y 'nombre_votante' como claves
plot_data_ideological <- inner_join(
  co_sponsor_counts,
  ordenamiento_1D_WNOM_01_15,
  by = c("convencional" = "nombre_votante")
)


pdf("scripts - plots/n_colaboracion_por_convencional_izq-der.pdf", width = 10, height = 6)

ggplot(plot_data_ideological, 
       # reorder() ahora usa 'posicion_izq_der'
       aes(x = reorder(convencional, posicion_izq_der), y = unique_co_sponsors)) +
  geom_col(fill = "darkred", width = 0.7, alpha=0.9) + # color para diferenciarlo del primer gráfico
  labs(
    title = "N° de colaboradores por Convencional",
    subtitle = "Ordenado por posición ideológica, bloques 01-15",
    x = NULL,
    y = "N° de Colaboradores Únicos"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15))
  )

dev.off()