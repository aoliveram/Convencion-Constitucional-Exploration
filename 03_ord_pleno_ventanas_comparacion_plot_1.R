library(ggplot2)
library(dplyr)
library(readr)

# Cargamos datos
orden_votantes_t <- read_csv("03_orden_votantes_t.csv")

# 1. Seleccionamos sesiones a ser comparadas
sesiones_comparar <- c("01-15 vs 16-21", "01-15 vs 76-99")
orden_votantes_t_filtrado <- orden_votantes_t %>%
  filter(comparacion %in% sesiones_comparar)

# 2. Calcular la posición final y añadir columnas útiles
orden_votantes_t_filtrado_plot <- orden_votantes_t_filtrado %>%
  mutate(
    # La posición inicial es la 'posicion_ideologica' de la ventana 'Periodo'
    posicion_inicial = posicion_ideologica,
    # La posición final es la inicial más la diferencia
    posicion_final = posicion_ideologica + diferencia_media,
    # Dirección del cambio
    direccion_cambio = case_when(
      diferencia_media > 0 ~ "Derecha",
      diferencia_media < 0 ~ "Izquierda",
      TRUE ~ "Sin cambio" # Para diferencias exactamente 0
    ),
    # Indicador de significancia
    significativo = p_valor < 0.05,
    # Etiqueta para la forma del punto
    etiqueta_significancia = ifelse(significativo, "Significativo (p < 0.05)", "No Significativo (p >= 0.05)")
  ) %>%
  # Asegurar el orden correcto de los factores para las leyendas
  mutate(
    direccion_cambio = factor(direccion_cambio, levels = c("Izquierda", "Derecha", "Sin cambio")),
    etiqueta_significancia = factor(etiqueta_significancia, levels = c("Significativo (p < 0.05)", "No Significativo (p >= 0.05)"))
  )

# --- Crear el gráfico ---

plot_pos_ideologica_final <- ggplot(orden_votantes_t_filtrado_plot, aes(x = reorder(Votante, posicion_inicial), y = posicion_inicial)) +
  
  # Dibuja el segmento desde la posición inicial a la final
  geom_segment(
    aes(
      xend = reorder(Votante, posicion_inicial),
      yend = posicion_final,
      color = direccion_cambio
    ),
    arrow = arrow(length = unit(0.15, "cm"), type = "closed"), # Añade una flecha
    linewidth = 0.8 # Grosor de la línea/flecha
  ) +
  
  # Marca el punto de inicio, con forma según significancia
  geom_point(
    aes(shape = etiqueta_significancia),
    size = 2.25,
    fill = "darkgrey", # Relleno blanco para formas como 21
    stroke = 0.8 # Borde del punto
  ) +
  
  # Facetas para cada comparación
  facet_wrap(~ comparacion, ncol = 1, scales = "free_x") + # ncol=1 para poner uno sobre otro
  # Escalas y etiquetas personalizadas
  scale_color_manual(
    name = "Dirección del Cambio",
    values = c("Izquierda" = "red", "Derecha" = "blue", "Sin cambio" = "grey"),
    drop = FALSE # Mantiene "Sin cambio" aunque no haya datos
  ) +
  scale_shape_manual(
    name = "Significancia Estadística",
    values = c("Significativo (p < 0.05)" = 21, "No Significativo (p >= 0.05)" = 22), # Círculo relleno vs cuadrado relleno
    drop = FALSE
  ) +
  scale_y_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, by = 0.5)) + # Ajusta límites y marcas del eje Y
  # Etiquetas y Título
  labs(
    title = "Cambio en la Posición Política de Convencionales",
    subtitle = "Comparación posición ideológica con W-Nominate, según ventanas de sesiones",
    x = "Votante (por posición inicial en 01-15)",
    y = "Posición Política (-1 Izquierda, +1 Derecha)"
  ) +
  # Tema
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), # Rota etiquetas eje X
    legend.position = "bottom", # Mueve la leyenda abajo
    legend.box = "vertical", # Organiza leyendas verticalmente
    panel.grid.major.x = element_blank(), # Quita líneas de grid verticales mayores
    panel.grid.minor.x = element_blank(), # Quita líneas de grid verticales menores
    strip.text = element_text(face = "bold", size = 11) # Estilo del título de faceta
  )

# --- Mostrar el gráfico ---
print(plot_pos_ideologica_final)

# --- Guardar el gráfico (opcional) ---
ggsave("comparacion_cambio_posiciones.png", plot_pos_ideologica_final, width = 12, height = 10, units = "in", dpi = 300)