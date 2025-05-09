# --- Load Required Libraries ---
library(readr)
library(data.table)
library(dplyr)
library(stringr)
library(purrr)
library(stringi)

# --- Helper Functions ---

# Function to clean legislator names
normalizar_nombres <- function(texto) {
  texto_limpio <- stri_trans_general(texto, "Latin-ASCII")
  return(texto_limpio)
}

# Function to rescale vector to [-1, 1]
reescalar <- function(vector_original) {
  # Add na.rm=TRUE for robustness
  min_original <- min(vector_original, na.rm = TRUE)
  max_original <- max(vector_original, na.rm = TRUE)
  a <- -1
  b <- 1
  # Handle case where all values are the same (prevents division by zero)
  if (min_original == max_original) {
    return(rep(0, length(vector_original))) # Assign 0 if range is zero
  }
  vector_reescalado <- ((vector_original - min_original) / (max_original - min_original)) * (b - a) + a
  return(vector_reescalado)
}

# --- Configuration ---
periods <- c("01-15", "16-21", "22-37", "56-75", "76-99", "100-106") # Adjust as needed
base_path <- "data - pleno/ordenamientos_pleno"
output_file <- "scripts - files/scripts - files/03_orden_votantes_t_MCMC.csv"

# --- 1. Load, Combine, and Rescale MCMC Sample Data ---
cat("Loading and rescaling MCMC sample data...\n")
combined_samples_dt <- rbindlist(
  lapply(periods, function(period) {
    file_suffix <- period
    filename <- file.path(base_path, paste0("ordenamiento_1D_MCMC_", file_suffix, "_samples.csv"))
    if (file.exists(filename)) {
      dt <- fread(filename)
      dt[, Periodo := period]
      dt[, legislador := normalizar_nombres(legislador)]
      # Only keep necessary columns for the test
      dt <- dt[, .(Periodo, iteracion, legislador, coord1D)]
      return(dt)
    } else {
      cat("WARNING: Sample file not found, skipping:", filename, "\n")
      return(NULL)
    }
  }), use.names = TRUE, fill = TRUE
)

if (nrow(combined_samples_dt) == 0) stop("No MCMC sample files loaded.")

# **Rescale coord1D within each Periodo**
combined_samples_dt[, coord1D := reescalar(coord1D), by = Periodo]
cat("MCMC sample data loaded and rescaled.\n")

# --- 2. Load, Combine, and Rescale Mean Position Data ---
cat("\nLoading and rescaling mean position data...\n")
combined_means_dt <- rbindlist(
  lapply(periods, function(period) {
    file_suffix <- period
    filename <- file.path(base_path, paste0("ordenamiento_1D_MCMC_", file_suffix, ".csv"))
    if (file.exists(filename)) {
      dt <- fread(filename)
      if (!all(c("nombre_votante", "posicion_ideologica") %in% names(dt))) {
        cat("ERROR: Mean file", filename, "missing required columns. Skipping.\n")
        return(NULL)
      }
      dt[, Periodo := period]
      dt[, Votante := normalizar_nombres(nombre_votante)]
      # Select, rename, and keep only needed columns
      dt <- dt[, .(Votante, Periodo, posicion_ideologica)]
      return(dt)
    } else {
      cat("WARNING: Mean position file not found, skipping:", filename, "\n")
      return(NULL)
    }
  }), use.names = TRUE, fill = TRUE
)

if (nrow(combined_means_dt) == 0) stop("No mean position files loaded.")

# **Rescale posicion_ideologica within each Periodo**
combined_means_dt[, posicion_ideologica := reescalar(posicion_ideologica), by = Periodo]
cat("Mean position data loaded and rescaled.\n")


# --- 3. Define Pairwise Period Combinations ---
period_combinations <- combn(periods, 2, simplify = FALSE)

# --- 4. Function for Paired T-Test (using rescaled data) ---
realizar_t_test_mcmc <- function(leg_name, period_combo, sample_data) {
  p1 <- period_combo[1]
  p2 <- period_combo[2]
  samples1 <- sample_data[legislador == leg_name & Periodo == p1, coord1D] # Uses rescaled coord1D
  samples2 <- sample_data[legislador == leg_name & Periodo == p2, coord1D] # Uses rescaled coord1D
  
  # Basic checks and error handling remain important
  if (length(samples1) < 2 || length(samples2) < 2 || length(samples1) != length(samples2)) {
    cat("  Skipping t-test for", leg_name, "between", p1, "and", p2, "- insufficient/unequal samples.\n")
    return(data.table(comparacion = paste(p1, "vs", p2), dif_media_t = NA_real_, p_valor = NA_real_))
  }
  tryCatch({
    t_result <- t.test(samples1, samples2, paired = TRUE)
    return(data.table(comparacion = paste(p1, "vs", p2), dif_media_t = t_result$estimate, p_valor = t_result$p.value))
  }, error = function(e) {
    cat("    ERROR running t.test for", leg_name, "between", p1, "and", p2, ":", conditionMessage(e),"\n")
    return(data.table(comparacion = paste(p1, "vs", p2), dif_media_t = NA_real_, p_valor = NA_real_))
  })
}

# --- 5. Perform T-Tests ---
cat("\nPerforming pairwise t-tests...\n")
unique_legislators <- unique(combined_samples_dt$legislador)
t_test_results_list <- lapply(unique_legislators, function(leg) {
  leg_results <- rbindlist(lapply(period_combinations, function(combo) {
    realizar_t_test_mcmc(leg, combo, combined_samples_dt)
  }))
  leg_results[, Votante := leg]
  return(leg_results)
})
orden_votantes_t_mcmc <- rbindlist(t_test_results_list)
setcolorder(orden_votantes_t_mcmc, "Votante")
cat("Finished t-tests.\n")

# --- 6. Add Mean Positions (Now Rescaled) and Final Formatting ---
cat("\nAdding mean positions and formatting...\n")
orden_votantes_t_mcmc[, c("Periodo1", "Periodo2") := tstrsplit(comparacion, " vs ", fixed = TRUE)]

# Merge to get initial rescaled position
orden_votantes_t_mcmc <- merge(
  orden_votantes_t_mcmc,
  combined_means_dt[, .(Votante, Periodo, pos_ideol_inicial = posicion_ideologica)], # Already rescaled
  by.x = c("Votante", "Periodo1"), by.y = c("Votante", "Periodo"), all.x = TRUE, sort = FALSE
)

# Merge to get final rescaled position
orden_votantes_t_mcmc <- merge(
  orden_votantes_t_mcmc,
  combined_means_dt[, .(Votante, Periodo, pos_ideol_final = posicion_ideologica)], # Already rescaled
  by.x = c("Votante", "Periodo2"), by.y = c("Votante", "Periodo"), all.x = TRUE, sort = FALSE
)

# Calculate difference between the *rescaled* means
orden_votantes_t_mcmc[, dif_media := pos_ideol_final - pos_ideol_inicial]

# Select and reorder final columns
orden_votantes_t_mcmc <- orden_votantes_t_mcmc[, .(
  Votante, comparacion, Periodo1, Periodo2,
  pos_ideol_inicial, pos_ideol_final, dif_media,
  dif_media_t, p_valor
)]

# --- 7. Save the Result ---
cat("\nSaving final comparison results to:", output_file, "\n")
fwrite(orden_votantes_t_mcmc, output_file, row.names = FALSE)

saveRDS(orden_votantes_t_mcmc, "scripts - files/scripts - files/03_orden_votantes_t_MCMC.rds")

orden_votantes_t_mcmc <- readRDS("scripts - files/scripts - files/03_orden_votantes_t_MCMC.rds")

cat("--- Analysis Complete ---\n")

# --- 8. Recoding 'Votante' ---

transformar_nombre_votante_revisado <- function(nombre) {
  
  # 1. Definir el mapa de reemplazo de caracteres especiales
  caracteres_especiales <- c(
    "~n" = "n", "~N" = "N",
    "'a" = "a", "'e" = "e", "'i" = "i", "'o" = "o", "'u" = "u",
    "'A" = "A", "'E" = "E", "'I" = "I", "'O" = "O", "'U" = "U"
  )
  
  # Limpiar caracteres especiales en toda la cadena (apellidos y nombres)
  nombre_limpio_completo <- str_replace_all(nombre, caracteres_especiales)
  
  # 2. Separar por ", " en apellido(s) y nombre(s)
  partes <- str_split(nombre_limpio_completo, ", ", n = 2)[[1]] # Obtener el vector directamente
  
  # Verificar si el split fue exitoso
  if (length(partes) != 2) {
    warning(paste("Formato inesperado para:", nombre))
    # Decide how to handle errors - maybe return original name after cleaning special chars
    return(nombre_limpio_completo)
  }
  
  apellido_partes_limpio <- str_trim(partes[1]) # La parte de los apellidos limpiada
  nombre_parte_limpio <- str_trim(partes[2])   # La parte de los nombres limpiada
  
  # 3. Extraer el primer apellido principal, manejando casos compuestos
  primer_apellido_principal <- ""
  palabras_apellido <- str_split(apellido_partes_limpio, " ")[[1]] # Separar la parte de apellidos por espacios
  
  # Caso 1: Apellido "De la Maza"
  if (length(palabras_apellido) >= 3 &&
      palabras_apellido[1] == "De" &&
      palabras_apellido[2] == "la" &&
      palabras_apellido[3] == "Maza") {
    primer_apellido_principal <- paste(palabras_apellido[1:3], collapse = " ") # Mantiene "De la Maza"
  }
  # Caso 2: Apellido "San Juan" (basado en la discrepancia encontrada)
  else if (length(palabras_apellido) >= 2 &&
           palabras_apellido[1] == "San" &&
           palabras_apellido[2] == "Juan") {
    primer_apellido_principal <- paste(palabras_apellido[1:2], collapse = " ") # Mantiene "San Juan"
  }
  # Caso por defecto: Tomar solo la primera palabra como el apellido principal
  # Esto funciona para la mayoría de los apellidos compuestos tipo "Apellido1 Apellido2"
  else {
    primer_apellido_principal <- palabras_apellido[1]
  }
  
  # 4. Combinar el primer apellido principal con la parte de los nombres limpios
  nombre_transformado <- paste0(primer_apellido_principal, ", ", nombre_parte_limpio)
  
  # 5. APLICAR CORRECCIONES ESPECÍFICAS OBSERVADAS EN TU LISTA TARGET (Opcional)
  # Estas son correcciones que no siguen una regla general del formato original,
  # sino que parecen ser normalizaciones o ediciones manuales en tu lista objetivo.
  # Inclúyelas SOLO si necesitas que el resultado coincida EXACTAMENTE con tu lista target.
  
  # Corrección "Dayyana" -> "Dayana"
  if (nombre_transformado == "Gonzalez, Dayyana") {
    nombre_transformado <- "Gonzalez, Dayana"
  }
  
  # Corrección "Loreto" -> "Rossana" para Vidal
  # Esta corrección cambia el nombre de pila, lo cual es inusual para una
  # transformación de formato. Asumo que es un caso específico que necesitas igualar.
  if (nombre_transformado == "Vidal, Loreto") { # Verifica el nombre transformado
    nombre_transformado <- "Vidal, Rossana"
  }
  # Nota: Si hay otros casos similares (cambios de nombre de pila no derivados),
  # necesitarías añadir reglas específicas para ellos aquí.
  
  return(nombre_transformado)
}

# Aplicamos la función revisada
orden_votantes_t_mcmc <- orden_votantes_t_mcmc %>%
  mutate(Votante = sapply(Votante, transformar_nombre_votante_revisado, USE.NAMES = FALSE))

# --- Verificar el resultado ---

votantes_transformados_revisado_unique <- unique(orden_votantes_t_mcmc$Votante)
votantes_target_unique <- unique(orden_votantes_t$Votante) # Original

print("¿Están todos los nombres transformados en la lista target?")
print(all(votantes_transformados_revisado_unique %in% votantes_target_unique))

print("¿Están todos los nombres de la lista target en los transformados?")
print(all(votantes_target_unique %in% votantes_transformados_revisado_unique))

# --- Guardamos ---

fwrite(orden_votantes_t_mcmc, output_file, row.names = FALSE)
saveRDS(orden_votantes_t_mcmc, "scripts - files/scripts - files/03_orden_votantes_t_MCMC.rds")

orden_votantes_t_mcmc <- readRDS("scripts - files/scripts - files/03_orden_votantes_t_MCMC.rds")
