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
output_file_comparison <- "03_comparacion_non-parametric_MCMC_vs_WNOMINATE.csv"
output_rds_comparison <- "03_comparacion_non-parametric_MCMC_vs_WNOMINATE.rds"

# --- 3. Configurar Procesamiento Paralelo ---
# Detectar número de cores (dejar uno libre es una buena práctica)

cl <- makeCluster(8, type = "FORK") 
registerDoParallel(cl)

# Iniciar temporizador
start_time <- Sys.time()

# --- 4. Procesamiento Paralelo por Período ---
cat(paste("Procesando", length(periods), "períodos en paralelo...\n"))

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

# --- Plot generation ---------------------------------------------------------

# --- 0. Cargar Librerías Necesarias ---
library(ggplot2)
library(data.table)
library(dplyr)     # Para filter, arrange, select si prefieres
library(forcats)   # Para fct_reorder
library(stringr)   # Para str_wrap si los nombres son muy largos

# --- 1. Cargar Datos ---
file_path <- "03_comparacion_non-parametric_MCMC_vs_WNOMINATE.rds" # Asegúrate que la ruta es correcta
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

ggsave("scripts - plots/comparacion_MCMC_vs_WNOM_p_value.png", plot = p_value_plot, bg = 'white', width = 18, height = 10, dpi = 550)
