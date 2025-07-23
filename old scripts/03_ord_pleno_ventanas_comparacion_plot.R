library(ggplot2)
library(dplyr)
library(readr)
library(patchwork) # para combinar gráficos

# Cargamos datos
orden_votantes_t <- read_csv("scripts - files/03_orden_votantes_t.csv")

# Objeto con toda la información para plot
orden_votantes_t_procesado <- orden_votantes_t %>%
  # Agregamos nuevas columnas
  mutate(
    posicion_inicial = posicion_ideologica,
    posicion_final = posicion_ideologica + diferencia_media,
    direccion_cambio = case_when(
                          diferencia_media > 0 ~ "Derecha",
                          diferencia_media < 0 ~ "Izquierda",
                          TRUE ~ "Sin cambio" # por si acaso
                                ),
    significativo = p_valor < 0.05,
    etiqueta_significancia = ifelse(significativo, "Significativo (p < 0.05)", "No Significativo (p >= 0.05)")
  ) %>%
  # Algunos más para plot
  mutate(
    direccion_cambio_factor = factor(direccion_cambio, levels = c("Izquierda", "Derecha")), 
    etiqueta_significancia_factor = factor(etiqueta_significancia, levels = c("Significativo (p < 0.05)", "No Significativo (p >= 0.05)"))
  )

# Fijamos escalas y formas para ambos plots consistentes
common_y_scale <- scale_y_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, by = 0.5))
common_color_scale <- scale_color_manual(
  name = "Dirección del Cambio",
  values = c("Izquierda" = "red", "Derecha" = "blue"),
  na.translate = FALSE # No mostrar NA si "Sin cambio" no existe
)
common_shape_scale <- scale_shape_manual(
  name = "Significancia Estadística",
  values = c(
    "Significativo (p < 0.05)" = 21,    # círculo 
    "No Significativo (p >= 0.05)" = 24 # triángulo hacia arriba 
  ),
  drop = FALSE
)
common_fill_scale <- scale_fill_manual(
  name = "Significancia Estadística",
  values = c(
    "Significativo (p < 0.05)" = "white", # Círculo blanco
    "No Significativo (p >= 0.05)" = "black"  # Triángulo rojo
  ),
  drop = FALSE
)
common_guides <- guides(
  shape = guide_legend(title = "Significancia Estadística"),
  fill = guide_legend(title = "Significancia Estadística"),
  color = guide_legend(title = "Dirección del Cambio")
)

# --- Crear plot 1 (01-15 vs 16-21) ---

data_plot_1 <- orden_votantes_t_procesado %>% filter(comparacion == "01-15 vs 16-21")

plot_1 <- ggplot(data_plot_1, aes(x = reorder(Votante, posicion_inicial), y = posicion_inicial)) +
  geom_segment(
    aes(xend = reorder(Votante, posicion_inicial), yend = posicion_final, color = direccion_cambio_factor),
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth = 0.8
  ) +
  geom_point(
    aes(shape = etiqueta_significancia_factor, fill = etiqueta_significancia_factor),
    size = 2.75, stroke = 0.5, color = "darkgrey" # Borde negro por defecto
  ) +
  common_y_scale +
  common_color_scale +
  common_shape_scale +
  common_fill_scale +
  common_guides +
  labs(
    title = "01-15 vs 16-21", # Título plot 1
    x = NULL, # sin labels eje-x
    y = "Posición Política"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11), # Título del subplot
    axis.text.x = element_blank(),   # sin texto en eje-x
    axis.ticks.x = element_blank(),  # quitamos marcas del eje X
    legend.position = "none",        # la leyenda se coloca al final
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2), # Grilla vertical sutil
    panel.grid.minor.x = element_blank()
  )

# --- Crear plot 2 (01-15 vs 76-99) ---

data_plot_2 <- orden_votantes_t_procesado %>% filter(comparacion == "01-15 vs 76-99")

plot_2 <- ggplot(data_plot_2, aes(x = reorder(Votante, posicion_inicial), y = posicion_inicial)) +
  geom_segment(
    aes(xend = reorder(Votante, posicion_inicial), yend = posicion_final, color = direccion_cambio_factor),
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth = 0.8
  ) +
  geom_point(
    aes(shape = etiqueta_significancia_factor, fill = etiqueta_significancia_factor),
    size = 2.50, stroke = 0.5, color = "darkgrey" # Borde negro por defecto
  ) +
  common_y_scale +
  common_color_scale +
  common_shape_scale +
  common_fill_scale +
  common_guides +
  labs(
    title = "01-15 vs 76-99", # Título específico
    x = "Votante (por posición inicial en 01-15)", # Mantenemos etiqueta eje-X
    y = "Posición Política"
  ) +
  theme_minimal(base_size = 15) + # Tamaño label 'Votante' en eje-x
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11), # Título de la subtrama
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), # Texto del eje-X
    legend.position = "none",        # La leyenda se colectará al final
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2), # Grilla vertical sutil
    panel.grid.minor.x = element_blank()
  )

# --- Combinar plots---

plot_combinado <- plot_1 / plot_2 + # El "/" apila verticalmente
  plot_layout(guides = 'collect') + # Colecta las leyendas de abajo
  plot_annotation( 
    title = "Cambio en la Posición Política de Convencionales",
    subtitle = "Comparación posición ideológica con W-Nominate, según ventanas de sesiones",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face="bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 12))
  ) 

print(plot_combinado)

# --- Guardamos el plot combinado ---
ggsave("scripts - plots/cambio_posiciones_pleno_1.png", plot_combinado, width = 16, height = 10, units = "in", dpi = 600)
