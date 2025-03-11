library(ggplot2)
library(dplyr)
library(scales)

# rescaling
original_min <- min(ordenamiento_wnom$legislators$coord1D)
original_max <- max(ordenamiento_wnom$legislators$coord1D)

ordenamiento_1D_wnom <- ordenamiento_wnom$legislators %>%
  mutate(
    posicion_ideologica = rescale(coord1D, to = c(-1, 1)),
    nombre_votante = votantes,
    legislador = factor(nombre_votante, levels = nombre_votante[order(posicion_ideologica)])
  )

resultados_bootstrap <- resultados_bootstrap %>%
  mutate(
    coord1D_rescaled = rescale(coord1D, to = c(-1, 1))
  )

# bootstrap statistics
bootstrap_summary <- resultados_bootstrap %>%
  group_by(legislador) %>%
  summarise(
    mean_pos = mean(coord1D_rescaled),
    ci_lower = quantile(coord1D_rescaled, 0.025),
    ci_upper = quantile(coord1D_rescaled, 0.975)
  ) %>%
  left_join(ordenamiento_1D_wnom, by = "legislador")

# create plot
ggplot(bootstrap_summary, aes(x = posicion_ideologica, y = reorder(legislador, posicion_ideologica))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                 height = 0.3, color = "#377eb8", 
                 linewidth = 0.4, alpha = 0.7) +
  geom_point(aes(x = posicion_ideologica), 
             color = "#e41a1c", size = 1.5, shape = 18) +
  labs(x = "Posición Ideológica (Izquierda-Derecha)",
       y = "Miembros de la Convención",
       title = "Estimaciones Ideológicas",
       subtitle = "Con intervalos de confianza del 95%. Puntos rojos: estimaciones W-NOMINATE. Barras azules: incertidumbre bootstrap",
       caption = "Fuente: votaciones al 14 de Agosto 2021") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 6, margin = margin(r = 5)),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    plot.caption = element_text(margin = margin(t = 15), color = "gray50")
  ) +
  scale_x_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, 0.5))
