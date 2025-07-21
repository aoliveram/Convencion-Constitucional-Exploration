# --- 1: TEST DE EQUIVALENCIA (TOST) ---
# Son "Equivalentes" si el IC del 95% está DENTRO de [-delta_rank, +delta_rank]
# 03_ord_pleno_MCMC_vs_WNOM_ordinal_test_hipotesis.R ya calculó el IC del 95%. 

library(data.table)

# 1. Resultados del test de Wilcoxon
file_path <- "scripts - files/03_comparacion_non-parametric_MCMC_vs_WNOMINATE.rds"
if (!file.exists(file_path)) stop("Archivo de resultados no paramétricos no encontrado.")
comparison_data <- readRDS(file_path)
setDT(comparison_data)

# 2. Definir la banda de equivalencia
delta_rank <- 8 # 18% de ventana: 0.05*154 = 7.7 ~ 8.

# 3. Aplicar la regla TOST
equivalencia_tost <- comparison_data[, .(
  legislador,
  Periodo,
  median_rank_mcmc,
  median_rank_wnom,
  diff_median_rank = median_rank_mcmc - median_rank_wnom,
  conf_int_low,
  conf_int_high,
  # Aplicar la lógica del test de equivalencia
  equivalente_tost = fifelse(
    !is.na(conf_int_low) & conf_int_low > -delta_rank & conf_int_high < delta_rank,
    "Sí", # Equivalente
    "No"  # No se puede concluir equivalencia
  )
)]

# 4. Resultados
print(head(equivalencia_tost))
print(table(equivalencia_tost$Periodo, equivalencia_tost$equivalente_tost))


# ------------------------------------------------------------------------------


# --- 2: HEATMAP DE EQUIVALENCIA (TOST) ---

library(ggplot2)
library(data.table)
library(forcats) # Para reordenar factores


# 1. Preparar los datos para el Heatmap
# Los ordenamos por el número de veces que sus posiciones fueron "No Equivalentes".
# Esto pondrá a los legisladores más inconsistentes juntos.

legislator_order <- equivalencia_tost[equivalente_tost == "No", .N, by = legislador][order(-N)]

# # Usamos rev() para que los que tienen más "No Equivalente" aparezcan arriba. (para el eje Y)
equivalencia_tost[, legislador_factor := factor(legislador, levels = rev(legislator_order$legislador))]

# Crear un factor para los períodos para asegurar el orden cronológico en el eje X
period_levels <- c("01-15", "16-21", "22-37", "56-75", "76-99", "100-106")
equivalencia_tost[, periodo_factor := factor(Periodo, levels = period_levels)]

# 2. Heatmap
heatmap_plot <- ggplot(
  equivalencia_tost, 
  aes(x = periodo_factor, y = legislador_factor, fill = equivalente_tost)
) +
  geom_tile(color = "white", linewidth = 0.5) + # geom_tile crea el heatmap
  scale_fill_manual(
    name = "Conclusión del Test",
    values = c("Sí" = "lightblue", "No" = "darkred"),
    labels = c(paste("Sí (Dif. <", delta_rank, "rangos)"), "No")
  ) +
  labs(
    title = "Concordancia Ordinal entre WNOMINATE e IDEAL",
    subtitle = paste("Test de Equivalencia TOST: IC 95% de la diferencia de rangos está dentro de una ventana de 10%."),
    x = "Período de Sesiones",
    y = "Convencional (Ordenado por Frecuencia de No Equivalencia)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 6), # Ajustar tamaño de nombres para que quepan
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    legend.position = "bottom"
  )

print(heatmap_plot)

# ggsave("scripts - plots/equivalencia_MCMC_vs_WNOM_tost_heatmap.png", plot = heatmap_plot, width = 10, height = 16, dpi = 300)

