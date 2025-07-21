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
delta_rank <- 16 # 5% de ventana: 0.025*154 = 3.85 ~ 4.

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

