# --- 0. Cargar Librerías Necesarias ---
library(data.table)
library(stringi)
library(parallel)
library(doParallel)
library(foreach)
library(dplyr) # Usaremos select y rename al final

# --- 1. Funciones Auxiliares ---

# Función para limpiar nombres de legisladores (reutilizada)
normalizar_nombres <- function(texto) {
  if (is.null(texto) || all(is.na(texto))) return(texto)
  texto_limpio <- stri_trans_general(as.character(texto), "Latin-ASCII")
  texto_limpio <- gsub("\"", "", texto_limpio)
  return(trimws(texto_limpio))
}

# --- 2. Configuración ---
base_path <- "data - pleno/ordenamientos_pleno" # Ajusta según tu estructura
periods <- c("01-15", "16-21", "22-37", "56-75", "76-99", "100-106") # Todos tus períodos
output_file_comparison <- "ideological-scaling-files/03_comparacion_non-parametric_MCMC_vs_WNOMINATE_orig.csv"
output_rds_comparison <- "ideological-scaling-files/03_comparacion_non-parametric_MCMC_vs_WNOMINATE_orig.rds"

# --- 3. Procesamiento Paralelo ---

cl <- makeCluster(8, type = "FORK") 
registerDoParallel(cl)

# Iniciar temporizador
start_time <- Sys.time()

# --- 4. Procesamiento Paralelo por Período ---

results_list <- foreach(
  period = periods,
  .packages = c('data.table', 'stringi'), # Paquetes necesarios dentro de cada worker
  .combine = 'rbind',    # Combina los data.tables resultantes por filas
  .errorhandling = 'pass' # Si un período falla, pasa al siguiente (registra el error)
) %dopar% {
  
  period_results <- data.table() # Inicializar data.table vacío para este período
  
  tryCatch({
    
    cat(paste("  Procesando período:", period, "...\n"))
    
    # --- 4.1 Cargar Datos del Período ---
    file_mcmc <- file.path(base_path, paste0("ordenamiento_1D_MCMC_", period, "_samples.csv"))
    file_wnom <- file.path(base_path, paste0("ordenamiento_1D_WNOM_", period, "_bootstrap.csv"))
    
    if (!file.exists(file_mcmc) || !file.exists(file_wnom)) {
      warning(paste("Archivos faltantes para el período", period, "- Omitiendo."))
      # Retorna un data.table vacío si faltan archivos para que rbind funcione
      return(data.table()) 
    }
    
    dt_mcmc <- fread(file_mcmc, encoding = "UTF-8")
    dt_wnom <- fread(file_wnom, encoding = "UTF-8")
    
    # Limpiar columna 'iteracion' si es necesario
    if ("iteracion" %in% names(dt_mcmc) && is.character(dt_mcmc$iteracion)) {
      dt_mcmc[, iteracion := as.integer(gsub(",$", "", iteracion))]
    }
    if ("iteracion" %in% names(dt_wnom) && is.character(dt_wnom$iteracion)) {
      # WNOM a veces tiene comillas dobles alrededor del número
      dt_wnom[, iteracion := as.integer(gsub("\"", "", iteracion))] 
    }
    
    # Seleccionar y renombrar columnas clave
    dt_mcmc <- dt_mcmc[, .(iteracion, legislador, coord1D_mcmc = coord1D)]
    dt_wnom <- dt_wnom[, .(iteracion, legislador, coord1D_wnom = coord1D)] # WNOM usa 'iteracion' para bootstrap replicate
    
    # --- 4.2 Normalizar Nombres ---
    dt_mcmc[, legislador := normalizar_nombres(legislador)]
    dt_wnom[, legislador := normalizar_nombres(legislador)]
    
    # Encontrar legisladores comunes a ambos métodos en este período
    common_legislators <- intersect(unique(dt_mcmc$legislador), unique(dt_wnom$legislador))
    if (length(common_legislators) == 0) {
      warning(paste("No hay legisladores comunes entre MCMC y WNOMINATE para el período", period))
      return(data.table())
    }
    dt_mcmc <- dt_mcmc[legislador %in% common_legislators]
    dt_wnom <- dt_wnom[legislador %in% common_legislators]
    
    # --- 4.3 Calcular Rangos por Iteración ---
    cat(paste("    Calculando rangos para período:", period, "...\n"))
    # Usar frankv para ranking rápido dentro de data.table, agrupado por iteracion
    # El ranking es ascendente (menor coord1D = menor rango = más a la izquierda)
    dt_mcmc[, rank_mcmc := frankv(coord1D_mcmc, ties.method = "average"), by = iteracion]
    dt_wnom[, rank_wnom := frankv(coord1D_wnom, ties.method = "average"), by = iteracion]
    
    # Seleccionar solo columnas necesarias para la comparación
    dt_mcmc_ranked <- dt_mcmc[, .(iteracion, legislador, rank_mcmc)]
    dt_wnom_ranked <- dt_wnom[, .(iteracion, legislador, rank_wnom)]
    
    # --- -- --- -- --- --- --- --- --- ---- --- -- --- --- -- - 
    # --- 4.3.1 Preparar Datos para Comparación de Pares ---
    # Seleccionar las columnas relevantes con los rangos por iteración
    dt_mcmc_ranked <- dt_mcmc[, .(iteracion, legislador, rank_mcmc)]
    dt_wnom_ranked <- dt_wnom[, .(iteracion, legislador, rank_wnom)]
    
    # --- 4.3.2 Generar Pares de Legisladores ---
    legisladores_periodo <- common_legislators # Usar los legisladores comunes ya identificados
    pares <- combn(legisladores_periodo, 2, simplify = FALSE) # Lista de pares [vector c(leg_A, leg_B)]
    cat(paste("    Generando", length(pares), "pares para comparación...\n"))
    
    # --- 4.3.3 Calcular Probabilidades de Orden Relativo por Par (en Paralelo dentro del Worker) ---
    # Nota: Estamos dentro de un worker %dopar% que ya está usando un core. 
    # La paralelización aquí es sobre los pares, no anidada.
    # El clúster principal ya fue definido afuera.
    
    cat(paste("    Calculando probabilidades de pares para período:", period, "...\n"))
    
    # Usamos lapply por simplicidad aquí, ya que foreach anidado puede ser complejo.
    # El bucle externo %dopar% sobre 'period' ya nos da la paralelización principal.
    resultados_pares_list <- lapply(pares, function(par) {
      leg_A <- par[1]
      leg_B <- par[2]
      
      # Extraer vectores de rangos para A y B
      ranks_A_mcmc <- dt_mcmc_ranked[legislador == leg_A, rank_mcmc]
      ranks_B_mcmc <- dt_mcmc_ranked[legislador == leg_B, rank_mcmc]
      ranks_A_wnom <- dt_wnom_ranked[legislador == leg_A, rank_wnom]
      ranks_B_wnom <- dt_wnom_ranked[legislador == leg_B, rank_wnom]
      
      # Asegurarse de que los vectores no estén vacíos (aunque no debería pasar con common_legislators)
      if (length(ranks_A_mcmc) == 0 || length(ranks_B_mcmc) == 0 || 
          length(ranks_A_wnom) == 0 || length(ranks_B_wnom) == 0) {
        warning(paste("Datos de rango faltantes para el par:", leg_A, "-", leg_B, "en período", period))
        return(NULL) 
      }
      
      # Calcular P(Rank_A < Rank_B | MCMC)
      # Usamos mean(condicion) que cuenta TRUEs como 1 y FALSEs como 0
      prob_A_lt_B_mcmc <- mean(ranks_A_mcmc < ranks_B_mcmc, na.rm = TRUE)
      
      # Calcular P(Rank_A < Rank_B | WNOM)
      prob_A_lt_B_wnom <- mean(ranks_A_wnom < ranks_B_wnom, na.rm = TRUE)
      
      # Calcular métricas de concordancia/discordancia
      diff_abs <- abs(prob_A_lt_B_mcmc - prob_A_lt_B_wnom)
      
      # Concordancia Direccional Simple: Ambos > 0.5 o ambos < 0.5 (o ambos == 0.5)
      # Usaremos una pequeña tolerancia para igualdad a 0.5
      tol <- 1e-6
      mcmc_dir <- fcase(prob_A_lt_B_mcmc > 0.5 + tol, 1, prob_A_lt_B_mcmc < 0.5 - tol, -1, default = 0)
      wnom_dir <- fcase(prob_A_lt_B_wnom > 0.5 + tol, 1, prob_A_lt_B_wnom < 0.5 - tol, -1, default = 0)
      concordancia_dir <- (mcmc_dir == wnom_dir)
      
      # Acuerdo con Certeza (ejemplo: ambos > 0.95 o ambos < 0.05)
      certeza_threshold <- 0.95
      mcmc_certain <- (prob_A_lt_B_mcmc > certeza_threshold | prob_A_lt_B_mcmc < (1 - certeza_threshold))
      wnom_certain <- (prob_A_lt_B_wnom > certeza_threshold | prob_A_lt_B_wnom < (1 - certeza_threshold))
      acuerdo_con_certeza <- (mcmc_certain & wnom_certain & concordancia_dir)
      desacuerdo_con_certeza <- (mcmc_certain & wnom_certain & !concordancia_dir)
      
      # Retornar data.table con resultados para este par
      data.table(
        Periodo = period, # Añadir período
        Legislador_A = leg_A, 
        Legislador_B = leg_B, 
        Prob_A_lt_B_MCMC = prob_A_lt_B_mcmc, 
        Prob_A_lt_B_WNOM = prob_A_lt_B_wnom, 
        Diferencia_Absoluta = diff_abs, 
        Concordancia_Direccional = concordancia_dir,
        Acuerdo_Certeza = acuerdo_con_certeza,
        Desacuerdo_Certeza = desacuerdo_con_certeza
      ) 
    })
    
    # Combinar la lista de resultados de pares en un solo data.table
    resultados_pares_dt <- rbindlist(resultados_pares_list)
    
    # - --- --- -- - -- ----- --- -- -- --- - - -- ---- ----- ---
    
    # --- 4.4 Combinar Datos de Rangos ---
    # Asegurar que tenemos el mismo número de iteraciones/bootstraps (o tomar el mínimo)
    # Aquí asumimos que son iguales (e.g., 1000), si no, habría que ajustar el merge/join
    # O hacer el análisis solo para las iteraciones presentes en ambos
    
    # Unir por legislador e iteración
    # Nota: Puede que las 'iteraciones' no coincidan directamente (MCMC sample vs Bootstrap replicate)
    # Si el número de muestras es el mismo (e.g., 1000), podemos forzar una unión, 
    # pero es conceptualmente una comparación de dos *distribuciones* independientes.
    # Por lo tanto, NO unimos por iteración. Procesamos por legislador.
    
    # Crear un data.table combinado con todas las muestras de rango
    combined_ranks_dt <- rbindlist(list(
      dt_mcmc_ranked[, .(legislador, method = "MCMC", rank = rank_mcmc)],
      dt_wnom_ranked[, .(legislador, method = "WNOMINATE", rank = rank_wnom)]
    ))
    
    # --- 4.5 Realizar Test U de Mann-Whitney (Wilcoxon) por Legislador ---
    cat(paste("    Realizando tests de Wilcoxon para período:", period, "...\n"))
    
    comparison_results <- combined_ranks_dt[, {
      ranks_mcmc <- rank[method == "MCMC"]
      ranks_wnom <- rank[method == "WNOMINATE"]
      
      # Calcular rangos medianos
      median_rank_mcmc <- median(ranks_mcmc, na.rm = TRUE)
      median_rank_wnom <- median(ranks_wnom, na.rm = TRUE)
      
      # Verificar si hay suficientes datos y varianza para el test
      valid_mcmc <- length(ranks_mcmc) >= 2 && var(ranks_mcmc, na.rm = TRUE) > 0
      valid_wnom <- length(ranks_wnom) >= 2 && var(ranks_wnom, na.rm = TRUE) > 0
      
      if (valid_mcmc && valid_wnom) {
        test_result <- tryCatch({
          # alternative="two.sided": H1 es que las distribuciones difieren
          # conf.int=TRUE para obtener estimador de diferencia de localización (pseudo-mediana)
          # exact=FALSE (o NULL por defecto para muestras grandes) para usar aproximación normal con corrección de continuidad
          wilcox.test(ranks_mcmc, ranks_wnom, paired = FALSE, 
                      alternative = "two.sided", conf.int = TRUE, exact = FALSE) 
        }, error = function(e) {
          cat("      Error en Wilcoxon test para:", legislador, "en Periodo:", period, "-", conditionMessage(e), "\n")
          NULL # Retorna NULL si hay error
        })
        
        if (!is.null(test_result)) {
          .(
            median_rank_mcmc = median_rank_mcmc,
            median_rank_wnom = median_rank_wnom,
            wilcox_p_value = test_result$p.value,
            W_statistic = test_result$statistic, # Estadístico W de Wilcoxon (o U de Mann-Whitney)
            location_shift_estimate = test_result$estimate, # Estimador Hodges-Lehmann de la diferencia
            conf_int_low = test_result$conf.int[1],
            conf_int_high = test_result$conf.int[2]
          )
        } else {
          # Si el test falló pero tenemos datos, al menos reportamos medianas
          .(median_rank_mcmc = median_rank_mcmc, median_rank_wnom = median_rank_wnom,
            wilcox_p_value = NA_real_, W_statistic = NA_real_, location_shift_estimate = NA_real_,
            conf_int_low = NA_real_, conf_int_high = NA_real_)
        }
      } else {
        # Si no hay datos suficientes o varianza
        .(median_rank_mcmc = if(length(ranks_mcmc)>0) median_rank_mcmc else NA_real_,
          median_rank_wnom = if(length(ranks_wnom)>0) median_rank_wnom else NA_real_,
          wilcox_p_value = NA_real_, W_statistic = NA_real_, location_shift_estimate = NA_real_,
          conf_int_low = NA_real_, conf_int_high = NA_real_)
      }
    }, by = .(legislador)] # Agrupar por legislador para hacer el test
    
    # Añadir la columna Periodo al resultado de este worker
    comparison_results[, Periodo := period]
    
    period_results <- comparison_results # Asignar al resultado del período
    
  }, error = function(e) {
    # Registrar error si algo falla en el procesamiento del período completo
    cat(paste("ERROR procesando período", period, ":", conditionMessage(e), "\n"))
    # Retorna data.table vacío para este período
    period_results <- data.table() 
  })
  
  return(period_results) # Retornar resultados del período (o vacío si falló)
}

# --- 5. Detener Clúster Paralelo ---
cat("\nDeteniendo clúster paralelo...\n")
stopCluster(cl)
registerDoSEQ() # Registrar backend secuencial de nuevo

# Imprimir tiempo de ejecución
end_time <- Sys.time()
cat("Tiempo de ejecución paralelo:", capture.output(end_time - start_time), "\n")






# --- 5.1 Calcular Correlación de Rangos (Spearman) por Período ---

# Asegurarse de que results_list no esté vacío
if (nrow(results_list) > 0) {
  
  # Usar data.table para agrupar por período y calcular correlación
  # Asegurarse de que las columnas de rango son numéricas
  results_list[, median_rank_mcmc := as.numeric(median_rank_mcmc)]
  results_list[, median_rank_wnom := as.numeric(median_rank_wnom)]
  
  # Calcular correlación y p-valor (si hay suficientes datos no NA)
  spearman_results <- results_list[, {
    # Filtrar NAs dentro del grupo antes de calcular cor.test
    valid_data <- .SD[!is.na(median_rank_mcmc) & !is.na(median_rank_wnom)]
    
    if (nrow(valid_data) >= 3) { # cor.test necesita al menos 3 pares válidos
      test_res <- tryCatch({
        cor.test(~ median_rank_mcmc + median_rank_wnom, data = valid_data, 
                 method = "spearman", exact = FALSE) # Usar aproximación para N > 10
      }, error = function(e) {
        cat(paste("  Error calculando Spearman para período", Periodo, ":", conditionMessage(e), "\n"))
        NULL 
      })
      
      if (!is.null(test_res)) {
        .(rho = test_res$estimate, p_value_rho = test_res$p.value, n_pairs = nrow(valid_data))
      } else {
        .(rho = NA_real_, p_value_rho = NA_real_, n_pairs = nrow(valid_data)) # Aún reportar N
      }
    } else {
      .(rho = NA_real_, p_value_rho = NA_real_, n_pairs = nrow(valid_data)) # Datos insuficientes
    }
  }, by = .(Periodo)] # Agrupar por Período
  
  # Mostrar resultados de la correlación
  cat("\nResultados de Correlación de Spearman (rho):\n")
  print(spearman_results)
  
  # Opcional: Guardar estos resultados específicos
  # fwrite(spearman_results, "05_correlacion_spearman_por_periodo.csv")
  
  # Opcional: Unir rho a la tabla principal (puede ser redundante si solo hay un valor por período)
  # results_list <- merge(results_list, spearman_results[, .(Periodo, rho)], by = "Periodo", all.x = TRUE)
  
} else {
  cat("ADVERTENCIA: No se pudieron calcular correlaciones porque results_list está vacío.\n")
  spearman_results <- data.table() # Crear tabla vacía para evitar errores posteriores
}



# --- 8. Visualización de Correlación de Spearman ---
cat("\nGenerando gráfico de correlación de Spearman...\n")
library(ggplot2) # Asegurar que ggplot2 está cargado

available_periods_sorted <- sort(spearman_results$Periodo)

if (nrow(spearman_results) > 0 && !all(is.na(spearman_results$rho))) {
  
  # Convertir Período a factor para ordenar en el gráfico
  spearman_results[, Periodo_factor := factor(Periodo, levels = available_periods_sorted)] # Usar el mismo orden de períodos que antes
  
  plot_spearman <- ggplot(spearman_results, aes(x = Periodo_factor, y = rho)) +
    geom_col(aes(fill = rho), show.legend = FALSE) + # Barras coloreadas por valor de rho
    geom_text(aes(label = sprintf("%.2f", rho)), vjust = -0.5, size = 3.5) + # Añadir valor de rho encima de la barra
    scale_fill_gradient2(low = "red", mid = "yellow", high = "darkgreen", midpoint = 0, limits=c(-1,1)) + # Escala de color divergente
    scale_y_continuous(limits = c(min(0, min(spearman_results$rho, na.rm=TRUE) - 0.1), 1.05), # Ajustar límite inferior si hay rhos negativos
                       breaks = seq(-1, 1, 0.25)) +
    labs(
      title = "Correlación Ordinal General (Spearman's Rho) entre WNOMINATE e IDEAL",
      subtitle = "Calculada sobre los rangos medianos de cada método por período",
      x = "Período de Sesiones",
      y = "Coeficiente Rho de Spearman"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1) # Rotar etiquetas si son muchas
    )
  
  print(plot_spearman)
  
  # Opcional: Guardar gráfico de Spearman
  # ggsave("plots_spearman/correlacion_spearman_periodos.png", plot = plot_spearman, width = 8, height = 6, dpi = 300)
  
} else {
  cat("No hay datos válidos de correlación para graficar.\n")
}














# --- 6. Procesar y Guardar Resultados Finales ---
if (nrow(results_list) > 0) {
  cat("Procesamiento final y guardado de resultados...\n")
  
  # Asegurar orden de columnas deseado
  setcolorder(results_list, c("legislador", "Periodo", "median_rank_mcmc", "median_rank_wnom", 
                              "wilcox_p_value", "W_statistic", "location_shift_estimate", 
                              "conf_int_low", "conf_int_high"))
  
  # Ordenar tabla final
  setorder(results_list, Periodo, legislador)
  
  # Comprobar salida
  print("Primeras filas de los resultados de la comparación no paramétrica:")
  print(head(results_list))
  print("Resumen de p-valores de Wilcoxon:")
  print(summary(results_list$wilcox_p_value))
  
  # Guardar
  fwrite(results_list, output_file_comparison, row.names = FALSE)
  saveRDS(results_list, output_rds_comparison)
  
  cat(paste("--- Análisis de comparación no paramétrica completado. Resultados guardados en:", output_file_comparison, "---\n"))
  
} else {
  cat("ERROR: No se generaron resultados. Verifique los logs y los archivos de entrada.\n")
}





# --- 7. Generar Gráficos de Dispersión de Pares (Fuera del bucle paralelo) ---
# Recalcular datos de pares fuera del bucle (más simple si los datos caben en memoria)**

library(ggplot2)
library(ggrepel) # Para etiquetas no solapadas

all_pairs_results_list <- list() # Para guardar los resultados de pares de todos los períodos

for (period in periods) {
  cat(paste("  Procesando pares para período:", period, "...\n"))
  # --- Re-cargar y Calcular Rangos (igual que dentro del foreach) ---
  file_mcmc <- file.path(base_path, paste0("ordenamiento_1D_MCMC_", period, "_samples.csv"))
  file_wnom <- file.path(base_path, paste0("ordenamiento_1D_WNOM_", period, "_bootstrap.csv"))
  if (!file.exists(file_mcmc) || !file.exists(file_wnom)) next # Saltar si faltan archivos
  
  dt_mcmc <- fread(file_mcmc, encoding = "UTF-8")
  dt_wnom <- fread(file_wnom, encoding = "UTF-8")
  if ("iteracion" %in% names(dt_mcmc) && is.character(dt_mcmc$iteracion)) dt_mcmc[, iteracion := as.integer(gsub(",$", "", iteracion))]
  if ("iteracion" %in% names(dt_wnom) && is.character(dt_wnom$iteracion)) dt_wnom[, iteracion := as.integer(gsub("\"", "", iteracion))]
  dt_mcmc <- dt_mcmc[, .(iteracion, legislador, coord1D_mcmc = coord1D)]
  dt_wnom <- dt_wnom[, .(iteracion, legislador, coord1D_wnom = coord1D)]
  dt_mcmc[, legislador := normalizar_nombres(legislador)]
  dt_wnom[, legislador := normalizar_nombres(legislador)]
  common_legislators <- intersect(unique(dt_mcmc$legislador), unique(dt_wnom$legislador))
  if (length(common_legislators) < 2) next # Necesitamos al menos 2 para formar pares
  dt_mcmc <- dt_mcmc[legislador %in% common_legislators]
  dt_wnom <- dt_wnom[legislador %in% common_legislators]
  dt_mcmc[, rank_mcmc := frankv(coord1D_mcmc, ties.method = "average"), by = iteracion]
  dt_wnom[, rank_wnom := frankv(coord1D_wnom, ties.method = "average"), by = iteracion]
  dt_mcmc_ranked <- dt_mcmc[, .(iteracion, legislador, rank_mcmc)]
  dt_wnom_ranked <- dt_wnom[, .(iteracion, legislador, rank_wnom)]
  
  # --- Calcular Pares (paralelizado con lapply/mclapply si se prefiere dentro del loop) ---
  legisladores_periodo <- common_legislators
  pares <- combn(legisladores_periodo, 2, simplify = FALSE)
  
  # Numero de cores
  num_cores_lapply <- detectCores() - 1 # Podrías usar los mismos cores
  if (num_cores_lapply < 1) num_cores_lapply <- 1
  
  # Usar lapply o mclapply (de PARALLEL, funciona con FORK) para acelerar dentro del loop
  resultados_pares_list_period <- mclapply(pares, mc.cores = num_cores_lapply, FUN = function(par) {
    # ... (mismo código que dentro de la función lapply anterior) ...
    leg_A <- par[1]
    leg_B <- par[2]
    ranks_A_mcmc <- dt_mcmc_ranked[legislador == leg_A, rank_mcmc]
    ranks_B_mcmc <- dt_mcmc_ranked[legislador == leg_B, rank_mcmc]
    ranks_A_wnom <- dt_wnom_ranked[legislador == leg_A, rank_wnom]
    ranks_B_wnom <- dt_wnom_ranked[legislador == leg_B, rank_wnom]
    if (length(ranks_A_mcmc) == 0 || length(ranks_B_mcmc) == 0 || length(ranks_A_wnom) == 0 || length(ranks_B_wnom) == 0) return(NULL)
    
    prob_A_lt_B_mcmc <- mean(ranks_A_mcmc < ranks_B_mcmc, na.rm = TRUE)
    prob_A_lt_B_wnom <- mean(ranks_A_wnom < ranks_B_wnom, na.rm = TRUE)
    diff_abs <- abs(prob_A_lt_B_mcmc - prob_A_lt_B_wnom)
    
    tol <- 1e-6
    mcmc_dir <- fcase(prob_A_lt_B_mcmc > 0.5 + tol, 1, prob_A_lt_B_mcmc < 0.5 - tol, -1, default = 0)
    wnom_dir <- fcase(prob_A_lt_B_wnom > 0.5 + tol, 1, prob_A_lt_B_wnom < 0.5 - tol, -1, default = 0)
    concordancia_dir <- (mcmc_dir == wnom_dir)
    certeza_threshold <- 0.95
    mcmc_certain <- (prob_A_lt_B_mcmc > certeza_threshold | prob_A_lt_B_mcmc < (1 - certeza_threshold))
    wnom_certain <- (prob_A_lt_B_wnom > certeza_threshold | prob_A_lt_B_wnom < (1 - certeza_threshold))
    acuerdo_con_certeza <- (mcmc_certain & wnom_certain & concordancia_dir)
    desacuerdo_con_certeza <- (mcmc_certain & wnom_certain & !concordancia_dir)
    data.table(Periodo = period, Legislador_A = leg_A, Legislador_B = leg_B, 
               Prob_A_lt_B_MCMC = prob_A_lt_B_mcmc, Prob_A_lt_B_WNOM = prob_A_lt_B_wnom, 
               Diferencia_Absoluta = diff_abs, Concordancia_Direccional = concordancia_dir,
               Acuerdo_Certeza = acuerdo_con_certeza, Desacuerdo_Certeza = desacuerdo_con_certeza)
  })
  
  resultados_pares_dt_period <- rbindlist(resultados_pares_list_period)
  all_pairs_results_list[[period]] <- resultados_pares_dt_period # Guardar resultados del período
  
  # --- Generar Gráfico de Dispersión para este período ---
  if (nrow(resultados_pares_dt_period) > 0) {
    # Crear etiqueta para pares problemáticos
    resultados_pares_dt_period[, label := fifelse(Diferencia_Absoluta > 0.5 | Desacuerdo_Certeza, 
                                                  paste(Legislador_A, "-", Legislador_B), 
                                                  "")]
    
    # Crear variable para colorear/dar forma según tipo de acuerdo/desacuerdo
    resultados_pares_dt_period[, status := fcase(
      Desacuerdo_Certeza, "Desacuerdo Fuerte",
      Acuerdo_Certeza, "Acuerdo Fuerte",
      Concordancia_Direccional, "Acuerdo Débil",
      !Concordancia_Direccional, "Desacuerdo Débil",
      default = "Indeterminado" # Si hay NAs o casos raros
    )]
    resultados_pares_dt_period[, status := factor(status, levels = c("Acuerdo Fuerte", "Acuerdo Débil", "Desacuerdo Débil", "Desacuerdo Fuerte", "Indeterminado"))]
    
    plot_pares <- ggplot(resultados_pares_dt_period, aes(x = Prob_A_lt_B_MCMC, y = Prob_A_lt_B_WNOM)) +
      geom_point(aes(color = status, shape = status), alpha = 0.6, size = 2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") + # Línea y=x
      geom_vline(xintercept = 0.5, linetype = "dotted", color = "grey70") + # Línea vertical en 0.5
      geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey70") + # Línea horizontal en 0.5
      geom_text_repel(aes(label = label), size = 2.5, max.overlaps = 15, 
                      box.padding = 0.4, point.padding = 0.2, segment.alpha = 0.5) +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      scale_color_manual(values = c("Acuerdo Fuerte" = "darkgreen", "Acuerdo Débil" = "lightblue", 
                                    "Desacuerdo Débil" = "orange", "Desacuerdo Fuerte" = "darkred", 
                                    "Indeterminado" = "grey"), name="Concordancia") +
      scale_shape_manual(values = c("Acuerdo Fuerte" = 16, "Acuerdo Débil" = 1, 
                                    "Desacuerdo Débil" = 2, "Desacuerdo Fuerte" = 17, 
                                    "Indeterminado" = 4), name="Concordancia") +
      labs(
        title = paste("Concordancia Ordinal por Pares - Período:", period),
        subtitle = "P(A < B | MCMC) vs P(A < B | WNOMINATE). Etiquetas para Diff Abs > 0.5 o Desacuerdo Fuerte.",
        x = "Probabilidad A < B (IDEAL/MCMC)",
        y = "Probabilidad A < B (WNOMINATE)"
      ) +
      coord_fixed() + # Asegura aspecto cuadrado
      theme_minimal(base_size = 10) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 8),
            legend.position = "bottom")
    
    print(plot_pares)
    # Opcional: Guardar gráfico
    # ggsave(paste0("plots_pares/concordancia_pares_", period, ".png"), plot = plot_pares, width = 8, height = 8.5, dpi = 300)
  } else {
    cat(paste("    No se generaron resultados de pares para el gráfico del período:", period, "\n"))
  }
}

# Combinar todos los resultados de pares en un solo data.table y guardarlo
# all_pairs_results_dt <- rbindlist(all_pairs_results_list)
# fwrite(all_pairs_results_dt, "04_comparacion_pares_todos_periodos.csv")
# saveRDS(all_pairs_results_dt, "04_comparacion_pares_todos_periodos.rds")








# --- Plot generation ---------------------------------------------------------

# --- 0. Cargar Librerías Necesarias ---
library(ggplot2)
library(data.table)
library(dplyr)     # Para filter, arrange, select si prefieres
library(forcats)   # Para fct_reorder
library(stringr)   # Para str_wrap si los nombres son muy largos

# --- 1. Cargar Datos ---
file_path <- "ideological-scaling-files/03_comparacion_non-parametric_MCMC_vs_WNOMINATE_orig.rds" # Asegúrate que la ruta es correcta
if (!file.exists(file_path)) {
  stop("El archivo ", file_path, " no se encuentra.")
}
comparison_data <- readRDS(file_path)
setDT(comparison_data) # Asegurar que es un data.table

# --- 2. Preparar Datos para el Gráfico ---

# Filtrar filas donde no se pudo calcular el p-valor
plot_data <- comparison_data[!is.na(wilcox_p_value)]

available_periods_sorted <- sort(unique(plot_data$Periodo))

# Usaremos el rango mediano WNOMINATE del primer período como referencia
reference_period <- available_periods_sorted[1] 

# Obtener el orden basado en median_rank_wnom del período de referencia
legislator_order <- plot_data[Periodo == reference_period][
  order(median_rank_wnom), # Ordenar por rango WNOMINATE mediano
  .(legislador, ordering_rank = median_rank_wnom) # Seleccionar legislador y el rango que define el orden
]

# Crear el factor ordenado para legislador
# Asegurarse que los niveles son únicos y en el orden correcto
ordered_legislators <- unique(legislator_order$legislador) 
plot_data[, legislador_factor := factor(legislador, levels = ordered_legislators)]

# Verificar si todos los legisladores en plot_data están en el factor ordenado
unique(plot_data$legislador[!plot_data$legislador %in% ordered_legislators])


# --- 3. Crear el Gráfico ---

# Definir colores para el gradiente (similar al ejemplo: claro para bajo p, oscuro para alto p)
low_color = "lightblue"
high_color = "darkblue"

p_value_plot <- ggplot(plot_data, aes(x = legislador_factor, y = wilcox_p_value, fill = wilcox_p_value)) +
  geom_col(show.legend = TRUE) + # Usar geom_col para barras cuya altura es el valor y
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ Periodo, ncol = 2, scales = "free_x") + # Paneles por período, 2 columnas
  scale_fill_gradient(low = low_color, high = high_color, name = "p-valor", limits=c(0,1)) +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.25), expand = c(0, 0)) + # Extender un poco el límite superior
  labs(
    title = "Contraste de Hipótesis: Comparación Ordinal WNOMINATE vs IDEAL",
    subtitle = paste("Test U de Mann-Whitney (Wilcoxon). Orden basado en Rango WNOMINATE Mediano del período", reference_period),
    x = "Convencional",
    y = "p-valor (Wilcoxon Test)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7), # Rotar y ajustar tamaño texto eje x
    axis.title.x = element_text(margin = margin(t = 10)), # Espacio para título eje x
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    strip.text = element_text(face = "bold"), # Título de los paneles (Período) en negrita
    panel.grid.major.x = element_blank(), # Ocultar líneas de grid verticales mayores
    panel.grid.minor.x = element_blank(), # Ocultar líneas de grid verticales menores
    panel.grid.minor.y = element_blank(),
    legend.position = "right" # Posición de la leyenda del gradiente
  )

# --- 4. Guardar el Gráfico ---

print(p_value_plot)

ggsave("ideological-scaling-plots/comparacion_MCMC_vs_WNOM_p_value.png", plot = p_value_plot, bg = 'white', width = 18, height = 10, dpi = 550)
