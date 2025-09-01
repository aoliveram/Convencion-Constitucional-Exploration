# # Este script genera 
# "scripts - files/03_comparacion_t_MCMC_vs_WNOMINATE.csv"
# "scripts - files/03_comparacion_t_MCMC_vs_WNOMINATE.rds"

# --- 0. Cargar Librerías Necesarias ---
library(data.table)
library(dplyr)
library(stringi)
library(purrr)

# --- 1. Funciones Auxiliares ---

# Función para limpiar nombres de legisladores
# normalizar_nombres <- function(texto) {
#   if (is.null(texto) || all(is.na(texto))) return(texto)
#   texto_limpio <- stri_trans_general(as.character(texto), "Latin-ASCII")
#   texto_limpio <- gsub("\"", "", texto_limpio) # Eliminar comillas dobles si aún persisten
#   return(trimws(texto_limpio))
# }

# Función para reescalar vector a [-1, 1]
reescalar <- function(vector_original) {
  if (all(is.na(vector_original)) || length(na.omit(vector_original)) < 1) {
    return(rep(NA_real_, length(vector_original)))
  }
  min_original <- min(vector_original, na.rm = TRUE)
  max_original <- max(vector_original, na.rm = TRUE)
  a <- -1
  b <- 1
  if (min_original == max_original) {
    return(rep(0, length(vector_original))) 
  }
  vector_reescalado <- ((vector_original - min_original) / (max_original - min_original)) * (b - a) + a
  return(vector_reescalado)
}

# --- 2. Configuración ---
# Asumimos que los archivos CSV de ejemplo están en el directorio de trabajo
# o ajusta base_path según sea necesario.
base_path <- "scripts - files/ordenamientos_pleno" # Directorio actual
periods <- c("01-15", "16-21", "22-37", "56-75", "76-99", "100-106")
output_file_comparison <- "scripts - files/03_comparacion_t_MCMC_vs_WNOMINATE.csv"
output_rds_comparison <- "scripts - files/03_comparacion_t_MCMC_vs_WNOMINATE.rds"

# --- 3. Cargar y Procesar Datos de Muestras (IDEAL y WNOMINATE) ---

# Función genérica para cargar y preprocesar datos de muestras
load_and_process_samples <- function(method_name, file_prefix, periods_list, base_dir) {
  cat(paste("Cargando y procesando datos de muestras para:", method_name, "...\n"))

  all_method_samples <- rbindlist(
    lapply(periods_list, function(period) {
      filename <- file.path(base_dir, paste0(file_prefix, period, 
                                             ifelse(method_name == "MCMC", "_samples.csv", "_bootstrap.csv")))
      if (file.exists(filename)) {
        dt <- fread(filename, encoding = "UTF-8")
        # A veces 'iteracion' puede venir con comillas y una coma, limpiar eso
        if ("iteracion" %in% names(dt) && is.character(dt$iteracion)) {
          dt[, iteracion := as.integer(gsub(",$", "", iteracion))]
        }
        dt[, Periodo := period]
        #dt[, legislador := normalizar_nombres(legislador)]
        dt[, legislador := legislador]
        # Seleccionar solo columnas necesarias
        dt <- dt[, .(Periodo, iteracion, legislador, coord1D)]
        return(dt)
      } else {
        cat("ADVERTENCIA: Archivo de muestras no encontrado, omitiendo:", filename, "\n")
        return(NULL)
      }
    }), use.names = TRUE, fill = TRUE
  )
  
  if (nrow(all_method_samples) == 0) {
    stop(paste("No se cargaron archivos de muestras para el método:", method_name))
  }
  
  # Reescala coord1D DENTRO de cada Periodo para este método
  all_method_samples[, coord1D_rescaled := reescalar(coord1D), by = .(Periodo)]
  all_method_samples[, method := method_name] # Añadir columna de método
  
  cat(paste("Datos de muestras para", method_name, "cargados y reescalados.\n"))
  return(all_method_samples[, .(Periodo, legislador, iteracion, method, coord1D_rescaled)])
}

# Cargar datos de MCMC (IDEAL)
mcmc_samples_dt <- load_and_process_samples(
  method_name = "MCMC",
  file_prefix = "ordenamiento_1D_MCMC_",
  periods_list = periods,
  base_dir = base_path
)

# Cargar datos de Bootstrap (WNOMINATE)
wnom_samples_dt <- load_and_process_samples(
  method_name = "WNOMINATE",
  file_prefix = "ordenamiento_1D_WNOM_",
  periods_list = periods,
  base_dir = base_path
)

# Combinar Datos de Muestras de Ambos Métodos ---
cat("\nCombinando datos de muestras de MCMC y WNOMINATE...\n")
combined_all_samples_dt <- rbindlist(list(mcmc_samples_dt, wnom_samples_dt), use.names = TRUE)


# --- 5. Realizar T-Tests para Muestras Independientes ---
cat("Realizando t-tests para muestras independientes...\n")

# Agrupar por legislador y período, luego realizar el test t
# Asegurarse de que coord1D_rescaled es numérico
if (!is.numeric(combined_all_samples_dt$coord1D_rescaled)) {
  combined_all_samples_dt[, coord1D_rescaled := as.numeric(coord1D_rescaled)]
}

t_test_results <- combined_all_samples_dt[, {
  samples_mcmc <- coord1D_rescaled[method == "MCMC"]
  samples_wnom <- coord1D_rescaled[method == "WNOMINATE"]
  
  # Verificar que hay suficientes datos para el test
  valid_mcmc <- length(samples_mcmc) >= 2 && var(samples_mcmc, na.rm = TRUE) > 0
  valid_wnom <- length(samples_wnom) >= 2 && var(samples_wnom, na.rm = TRUE) > 0
  
  if (valid_mcmc && valid_wnom) {
    test_result <- tryCatch({
      t.test(samples_mcmc, samples_wnom, paired = FALSE) # Muestras independientes
    }, error = function(e) {
      cat("  Error en t-test para:", legislador, "en Periodo:", Periodo, "-", conditionMessage(e), "\n")
      NULL
    })
    
    if (!is.null(test_result)) {
      .(
        t_statistic = test_result$statistic,
        p_value = test_result$p.value,
        df = test_result$parameter,
        conf_int_low = test_result$conf.int[1],
        conf_int_high = test_result$conf.int[2]
      )
    } else {
      .(
        t_statistic = NA_real_, p_value = NA_real_, df = NA_real_,
        conf_int_low = NA_real_, conf_int_high = NA_real_
      )
    }
  } else {
    cat("  Omitiendo t-test para:", legislador, "en Periodo:", Periodo, "- datos insuficientes o varianza cero.\n")
    .(
      t_statistic = NA_real_, p_value = NA_real_, df = NA_real_,
      conf_int_low = NA_real_, conf_int_high = NA_real_
    )
  }
}, by = .(legislador, Periodo)]


# --- 6. Cargar y Añadir Estimaciones Puntuales ---

load_point_estimates <- function(method_name, file_prefix_mean, periods_list, base_dir) {
  cat(paste("Cargando estimaciones puntuales para:", method_name, "...\n"))
  all_point_estimates <- rbindlist(
    lapply(periods_list, function(period) {
      filename <- file.path(base_dir, paste0(file_prefix_mean, period, ".csv")) # Asume extensión .csv
      if (file.exists(filename)) {
        dt <- fread(filename, encoding = "UTF-8")
        dt[, Periodo := period]

        if ("nombre_votante" %in% names(dt)) setnames(dt, "nombre_votante", "legislador")
        if ("posicion_ideologica" %in% names(dt)) setnames(dt, "posicion_ideologica", "coord1D_puntual")
        else if ("coord1D" %in% names(dt) && !"coord1D_puntual" %in% names(dt)) setnames(dt, "coord1D", "coord1D_puntual")

        #dt[, legislador := normalizar_nombres(legislador)]
        dt[, legislador := legislador]
        dt <- dt[, .(Periodo, legislador, coord1D_puntual)]
        return(dt)
      } else {
        cat("ADVERTENCIA: Archivo de estimación puntual no encontrado, omitiendo:", filename, "\n")
        return(NULL)
      }
    }), use.names = TRUE, fill = TRUE
  )

  if (nrow(all_point_estimates) > 0) {
    all_point_estimates[, coord1D_puntual_rescaled := reescalar(coord1D_puntual), by = .(Periodo)]
    setnames(all_point_estimates, "coord1D_puntual_rescaled", paste0("point_estimate_", tolower(method_name)))
    return(all_point_estimates[, .(Periodo, legislador, get(paste0("point_estimate_", tolower(method_name))))])
  }
  return(NULL)
}

# Cargar estimaciones puntuales MCMC
point_mcmc_dt <- load_point_estimates("MCMC", "ordenamiento_1D_MCMC_", periods, base_path)
setnames(point_mcmc_dt, "V3", "pos_ideol_mcmc") # data.table a veces nombra V1, V2 etc.

t_test_results <- merge(t_test_results, point_mcmc_dt, by = c("legislador", "Periodo"), all.x = TRUE)

# Cargar estimaciones puntuales WNOMINATE
point_wnom_dt <- load_point_estimates("WNOMINATE", "ordenamiento_1D_WNOM_", periods, base_path)
setnames(point_wnom_dt, "V3", "pos_ideol_wn")

# Merging

t_test_results <- merge(t_test_results, point_wnom_dt, by = c("legislador", "Periodo"), all.x = FALSE)

# Calcular diferencia de estimaciones puntuales si existen
if ("pos_ideol_mcmc" %in% names(t_test_results) && "pos_ideol_wn" %in% names(t_test_results)) {
  t_test_results[, dif_pos_wn_mcmc := pos_ideol_wn - pos_ideol_mcmc]
}

# --- 7. Ordenar y Guardar Resultados ---
cat("\nGuardando resultados de la comparación...\n")
setorder(t_test_results, Periodo, legislador)

# Comprobar el contenido antes de guardar
print("Primeras filas de los resultados del t-test:")
print(head(t_test_results))
print("Resumen de p-valores:")
print(summary(t_test_results$p_value))


fwrite(t_test_results, file.path(output_file_comparison), row.names = FALSE)
saveRDS(t_test_results, file.path(output_rds_comparison))
