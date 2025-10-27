# Creates volatilidad_ordenamiento_politico.pdf

# --- 0. Load Libraries ---
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(forcats)

# --- 1. Load Data -----------------------------------------------------------
tryCatch({
  orden_votantes_t_raw <- readRDS("ideological-scaling-files/03_orden_votantes_t.rds")
}, error = function(e) {
  stop("Error loading ideological-scaling-files/03_orden_votantes_t.rds. Make sure the file exists. Original error: ", e$message)
})

# --- 2. Identify the Correct Sequence of Periods ----------------------------
# Get all unique periods from the 'comparacion' column
all_periods <- unique(c(
  str_extract(orden_votantes_t_raw$comparacion, "^\\d{2}-\\d{2}(?=\\s)"),
  str_extract(orden_votantes_t_raw$comparacion, "(?<=\\s)[^\\s]+$")
))

# Sort them numerically to ensure chronological order
period_sorter <- tibble(Periodo = na.omit(all_periods)) %>%
  mutate(start_num = as.numeric(str_extract(Periodo, "^\\d+"))) %>%
  arrange(start_num)
periodos_unicos <- period_sorter$Periodo

# Create the list of exact sequential comparison strings to look for
sequential_comparisons <- paste(head(periodos_unicos, -1), "vs", tail(periodos_unicos, -1))

# --- 3. Calculate Volatility for Sequential Periods ONLY --------------------

volatility_data <- orden_votantes_t_raw %>%
  
  # PASO CLAVE: Filtrar primero para quedarse SOLO con las comparaciones secuenciales
  filter(comparacion %in% sequential_comparisons) %>%
  
  # Ahora, sobre este subconjunto de datos, aplicamos la lógica anterior
  filter(p_valor < 0.05) %>%
  
  # Agrupamos por cada comparación individual para calcular los rankings
  group_by(comparacion) %>%
  mutate(
    posicion_final_calc = pos_ideol_inicial + dif_media,
    rank_inicial = rank(-pos_ideol_inicial, ties.method = "first"),
    rank_final = rank(-posicion_final_calc, ties.method = "first"),
    diferencia_ordinal = rank_final - rank_inicial
  ) %>%
  
  # Volvemos a agrupar por comparación para sumarizar y calcular el índice
  group_by(comparacion) %>%
  summarise(
    indice_volatilidad = sum(abs(diferencia_ordinal)) / 2,
    .groups = 'drop' # Usar .groups = 'drop' es buena práctica
  ) %>%
  
  # Asegurar que el eje X del gráfico mantenga el orden cronológico
  mutate(comparacion = factor(comparacion, levels = sequential_comparisons))


# --- 4. Create the Plot -----------------------------------------------------

pdf("ideological-scaling-plots/volatilidad_ordenamiento_politico.pdf", width = 10, height = 6)

ggplot(volatility_data, aes(x = comparacion, y = indice_volatilidad)) +
  geom_col(fill = "#0072B2", alpha = 0.8, width = 0.7) +
  geom_text(
    aes(label = round(indice_volatilidad, 1)), 
    vjust = -0.7, 
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .1))
  ) +
  coord_cartesian(ylim = c(350, NA)) +
  labs(
    title = "Volatilidad del Ordenamiento Político",
    subtitle = "Suma de cambios de posición ordinal significativos (p < 0.05), corregido por doble conteo.",
    x = "Comparación de Bloques de Sesiones",
    y = "Índice de Volatilidad Ordinal"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas si se solapan
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

dev.off()

ggplot(volatility_data, aes(x = periodo_final, y = indice_volatilidad)) +
  geom_col(fill = "#0072B2", alpha = 0.8) + # Gráfico de columnas (barras)
  geom_text(aes(label = round(indice_volatilidad, 1)), vjust = -0.5, size = 4) + # Añadir valor sobre la barra
  labs(
    title = "Volatilidad del Ordenamiento Político por Bloque de Sesiones",
    subtitle = "Suma de todos los cambios de posición ordinal significativos (p < 0.05), corregido por doble conteo.",
    x = "Bloque de Sesiones (Comparado con el Anterior)",
    y = "Índice de Volatilidad Ordinal"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
