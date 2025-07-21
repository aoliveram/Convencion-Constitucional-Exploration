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


# ------------------------------------------------------------------------------


# --- 3: TEST DE EQUIVALENCIA (BAYESIANO) ---

library(data.table)
library(stringi)
library(parallel)
library(doParallel)
library(foreach)

# --- 1. Funciones y Configuración ---
normalizar_nombres <- function(texto) {
  if (is.null(texto) || all(is.na(texto))) return(texto)
  texto_limpio <- stri_trans_general(as.character(texto), "Latin-ASCII")
  texto_limpio <- gsub("\"", "", texto_limpio)
  return(trimws(texto_limpio))
}

base_path <- "scripts - files/ordenamientos_pleno"
periods <- c("01-15", "16-21", "22-37", "56-75", "76-99", "100-106")
# delta_rank <- 8  (Mismo delta_rank de TOST)
rope_probability_threshold <- 0.95 # Umbral para declarar equivalencia

# --- 2. Configurar Paralelismo ---
num_cores <- 8
cl <- makeCluster(num_cores)
registerDoParallel(cl)
cat(paste("Iniciando test de equivalencia Bayesiano en", num_cores, "cores...\n"))

# --- 3. Bucle Paralelo por Período ---
equivalencia_bayesiana <- foreach(
  period = periods,
  .packages = c('data.table', 'stringi'),
  .combine = 'rbind',
  .errorhandling = 'pass'
) %dopar% {
  
  cat(paste("  Procesando período:", period, "...\n"))
  
  # --- Cargar y procesar rangos ---
  file_mcmc <- file.path(base_path, paste0("ordenamiento_1D_MCMC_", period, "_samples.csv"))
  file_wnom <- file.path(base_path, paste0("ordenamiento_1D_WNOM_", period, "_bootstrap.csv"))
  if (!file.exists(file_mcmc) || !file.exists(file_wnom)) return(NULL)
  
  dt_mcmc <- fread(file_mcmc)[, legislador := normalizar_nombres(legislador)]
  dt_wnom <- fread(file_wnom)[, legislador := normalizar_nombres(legislador)]
  
  # Asegurar consistencia en el número de muestras
  n_samples <- min(dt_mcmc[, .N, by=legislador]$N, dt_wnom[, .N, by=legislador]$N)
  dt_mcmc <- dt_mcmc[, .SD[1:n_samples], by=legislador]
  dt_wnom <- dt_wnom[, .SD[1:n_samples], by=legislador]
  
  dt_mcmc[, rank_mcmc := frankv(coord1D, ties.method="average"), by=iteracion]
  dt_wnom[, rank_wnom := frankv(coord1D, ties.method="average"), by=iteracion]
  
  # --- Unir distribuciones y calcular ---
  dist_mcmc <- dt_mcmc[, .(legislador, rank_mcmc)][, sample_id := 1:.N, by = legislador]
  dist_wnom <- dt_wnom[, .(legislador, rank_wnom)][, sample_id := 1:.N, by = legislador]
  
  dist_combinada <- merge(dist_mcmc, dist_wnom, by = c("legislador", "sample_id"))
  
  equivalencia_bayesiana_periodo <- dist_combinada[, {
    diferencia_de_rangos <- rank_mcmc - rank_wnom
    prob_en_rope <- mean(abs(diferencia_de_rangos) <= delta_rank)
    .(prob_equivalencia = prob_en_rope)
  }, by = legislador]
  
  equivalencia_bayesiana_periodo[, Periodo := period]
  
  return(equivalencia_bayesiana_periodo)
}

stopCluster(cl)
registerDoSEQ()
cat("Cálculo Bayesiano completado.\n")

# Clasificar y ver resultados
equivalencia_bayesiana[, equivalente_bayes := fifelse(prob_equivalencia > rope_probability_threshold, "Sí", "No")]

# Unir con los resultados del TOST para una tabla comparativa
equivalencia_tabla_final <- merge(equivalencia_tost, equivalencia_bayesiana, by = c("legislador", "Periodo"))

print("Tabla Final de Tests de Equivalencia:")
print(head(equivalencia_tabla_final))
cat("\nResumen de Conclusiones TOST vs Bayesiano (para el primer período):\n")
print(table(equivalencia_tabla_final[Periodo == periods[1], .(TOST = equivalente_tost, Bayesiano = equivalente_bayes)]))
print(table(equivalencia_tabla_final[Periodo == periods[2], .(TOST = equivalente_tost, Bayesiano = equivalente_bayes)]))
print(table(equivalencia_tabla_final[Periodo == periods[3], .(TOST = equivalente_tost, Bayesiano = equivalente_bayes)]))
print(table(equivalencia_tabla_final[Periodo == periods[4], .(TOST = equivalente_tost, Bayesiano = equivalente_bayes)]))
print(table(equivalencia_tabla_final[Periodo == periods[5], .(TOST = equivalente_tost, Bayesiano = equivalente_bayes)]))
print(table(equivalencia_tabla_final[Periodo == periods[6], .(TOST = equivalente_tost, Bayesiano = equivalente_bayes)]))

# ------------------------------------------------------------------------------

# Suponiendo que has corrido el bayesiano para todos los períodos y lo has combinado en 'equivalencia_bayesiana_total'
tabla_final <- merge(equivalencia_tost, equivalencia_bayesiana, by = c("legislador", "Periodo"))
print(tabla_final)

# ------------------------------------------------------------------------------
