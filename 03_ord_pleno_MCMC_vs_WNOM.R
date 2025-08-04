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
normalizar_nombres <- function(texto) {
  if (is.null(texto) || all(is.na(texto))) return(texto)
  texto_limpio <- stri_trans_general(as.character(texto), "Latin-ASCII")
  texto_limpio <- gsub("\"", "", texto_limpio) # Eliminar comillas dobles si aún persisten
  return(trimws(texto_limpio))
}

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
base_path <- "data - pleno/ordenamientos_pleno" # Directorio actual
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
        dt[, legislador := normalizar_nombres(legislador)]
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

        dt[, legislador := normalizar_nombres(legislador)]
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

# inicio CAMBIAR NOMBRES EN t_test_results ----

name_mapping_dictionary <- c(
  "Abarca Gonz'alez, Damaris" = "Abarca, Damaris",
  "Abarca Riveros, Jorge" = "Abarca, Jorge",
  "Achurra D'iaz, Ignacio" = "Achurra, Ignacio",
  "Aguilera Hey, Tiare" = "Aguilera, Tiare",
  "Alvarado Jorquera, Gloria" = "Alvarado, Gloria",
  "Alvarez Pinto, Julio" = "Alvarez, Julio",
  "Alvarez Zenteno, Rodrigo" = "Alvarez, Rodrigo",
  "Alvez Mar'in, Amaya" = "Alvez, Amaya",
  "Ampuero Barrientos, Adriana" = "Ampuero, Adriana",
  "Andrade Le'on, Crist'obal" = "Andrade, Cristobal",
  "Antilef ~Nanco, Victorino" = "Antilef, Victorino", # Note: ~N -> N
  "Arancibia Reyes, Jorge" = "Arancibia, Jorge",
  "Arauna Urrutia, Francisca" = "Arauna, Francisca",
  "Arellano Ortega, Marco" = "Arellano, Marco",
  "Arrau Garc'ia-Huidobro, Mart'in" = "Arrau, Martin",
  "Atria Lemaitre, Fernando" = "Atria, Fernando",
  "Bacian Delgado, Wilfredo" = "Bacian, Wilfredo",
  "Baradit Morales, Jorge" = "Baradit, Jorge",
  "Baranda Ferr'an, Benito" = "Baranda, Benito",
  "Barcel'o Amado, Luis" = "Barcelo, Luis",
  "Barraza G'omez, Marcos" = "Barraza, Marcos",
  "Bassa Mercado, Jaime" = "Bassa, Jaime",
  "Botto Salinas, Miguel 'Angel" = "Botto, Miguel Angel", # Note: 'Angel
  "Bown Sep'ulveda, Carol" = "Bown, Carol",
  "Bravo Silva, Daniel" = "Bravo, Daniel",
  "C'espedes Fern'andez, Lorena" = "Cespedes, Lorena",
  "Caama~no Rojas, Francisco" = "Caamano, Francisco", # Note: ~n -> n
  "Caiguan Ancapan, Alexis" = "Caiguan, Alexis",
  "Calvo Mu~noz, Carlos" = "Calvo, Carlos", # Note: ~n -> n
  "Cancino Meneses, Adriana" = "Cancino, Adriana",
  "Cantuarias Rubio, Roc'io" = "Cantuarias, Rocio",
  "Carrillo Vidal, Alondra" = "Carrillo, Alondra",
  "Castillo Boilet, Mar'ia Trinidad" = "Castillo, Trinidad",
  "Castillo Vigouroux, Eduardo" = "Castillo, Eduardo",
  "Castro Guti'errez, Claudia" = "Castro, Claudia",
  "Catrileo Arias, Rosa" = "Catrileo, Rosa",
  "Celed'on Fern'andez, Roberto" = "Celedon, Roberto",
  "Celis Montt, Ra'ul" = "Celis, Raul",
  "Chah'in Valenzuela, Fuad" = "Chahin, Fuad",
  "Chinga Ferreira, Eric" = "Chinga, Eric",
  "Cozzi Elzo, Ruggero" = "Cozzi, Ruggero",
  "Cretton Rebolledo, Eduardo" = "Cretton, Eduardo",
  "Cruz Carrasco, Andr'es" = "Cruz, Andres",
  "Cubillos Sigall, Marcela" = "Cubillos, Marcela",
  "Daza Carrasco, Mauricio" = "Daza, Mauricio",
  "De la Maza Ba~nados, Bernardo" = "De la Maza, Bernardo", # Note: ~n -> n
  "Delgado Vergara, Aurora" = "Delgado, Aurora",
  "Dom'inguez Donoso, Gaspar" = "Dominguez, Gaspar",
  "Dorador Ortiz, Cristina" = "Dorador, Cristina",
  "Fern'andez Chadwick, Patricio" = "Fernandez, Patricio",
  "Flores Carlos, Alejandra" = "Flores, Alejandra",
  "Fontaine Talavera, Bernardo" = "Fontaine, Bernardo",
  "Fuchslocher Baeza, Javier" = "Fuchslocher, Javier",
  "G'omez Castro, Claudio" = "Gomez, Claudio",
  "G'omez S'anchez, Yarela" = "Gomez, Yarela",
  "Gallardo Prado, Bessy" = "Gallardo, Bessy",
  "Galleguillos Aymani, F'elix" = "Galleguillos, Felix",
  "Gar'in Gonz'alez, Renato" = "Garin, Renato",
  "Giustinianovich Campos, Elisa" = "Giustinianovich, Elisa",
  "Godoy Monardez, Isabel" = "Godoy, Isabel",
  "Gonz'alez Araya, Dayyana" = "Gonzalez, Dayana",
  "Gonz'alez Calder'on, Lidia" = "Gonzalez, Lidia",
  "Grand'on Caro, Giovanna" = "Grandon, Giovanna",
  "Grand'on Gonz'alez, Paola" = "Grandon, Paola",
  "Guti'errez G'alvez, Hugo" = "Gutierrez, Hugo",
  "Harboe Bascu~n'an, Felipe" = "Harboe, Felipe", # Note: ~n -> n
  "Henr'iquez Carre~no, Natalia" = "Henriquez, Natalia", # Note: ~n -> n
  "Hoppe Espoz, Vanessa" = "Hoppe, Vanessa",
  "Hube Portus, Constanza" = "Hube, Constanza",
  "Hurtado Olave, Ruth" = "Hurtado, Ruth",
  "Hurtado Roco, Maximiliano" = "Hurtado, Maximiliano",
  "Jim'enez C'aceres, Luis" = "Jimenez, Luis",
  "Jofr'e C'aceres, Alvaro" = "Jofre, Alvaro",
  "Jurgensen Caesar, Harry" = "Jurgensen, Harry",
  "Labb'e Salazar, Basti'an" = "Labbe, Bastian",
  "Labra Besserer, Patricia" = "Labra, Patricia",
  "Labra~na Pino, Elsa" = "Labrana, Elsa", # Note: ~n -> n
  "Laibe Saez, Tom'as" = "Laibe, Tomas",
  "Larra'in Matte, Hern'an" = "Larrain, Hernan",
  "Letelier Cort'es, Margarita" = "Letelier, Margarita",
  "Linconao Huircap'an, Francisca" = "Linconao, Francisca",
  "Llanquileo Pilquim'an, Natividad" = "Llanquileo, Natividad",
  "Logan Soto, Rodrigo" = "Logan, Rodrigo",
  "Loncon Antileo, Elisa" = "Loncon, Elisa",
  "Madriaga Flores, Tania" = "Madriaga, Tania",
  "Mamani Mamani, Isabella" = "Mamani, Isabella",
  "Marinovic Vial, Teresa" = "Marinovic, Teresa",
  "Mart'inez Llancapan, Helmuth" = "Martinez, Helmuth",
  "Martin Bravo, Juan Jos'e" = "Martin, Juan Jose",
  "Mayol Bouchon, Luis" = "Mayol, Luis",
  "Mella Escobar, Jeniffer" = "Mella, Jeniffer",
  "Mena Villar, Felipe" = "Mena, Felipe",
  "Meneses Palma, Janis" = "Meneses, Janis",
  "Millabur ~Nancuil, Adolfo" = "Millabur, Adolfo", # Note: ~N -> N
  "Miranda Arce, Valentina" = "Miranda, Valentina",
  "Monckeberg Bruner, Cristian" = "Monckeberg, Cristian",
  "Montealegre Navarro, Katerine" = "Montealegre, Katerine",
  "Montero Allende, Ricardo" = "Montero, Ricardo",
  "Moreno Echeverr'ia, Alfredo" = "Moreno, Alfredo",
  "Mu~noz Leiva, Pedro" = "Munoz, Pedro", # Note: ~n -> n
  "N'u~nez Gangas, Nicol'as" = "Nunez, Nicolas", # Note: ~n -> n
  "Namor Kong, Guillermo" = "Namor, Guillermo",
  "Navarrete Arratia, Geoconda" = "Navarrete, Geoconda",
  "Neumann Bertin, Ricardo" = "Neumann, Ricardo",
  "Olivares Miranda, Ivanna" = "Olivares, Ivanna",
  "Orellana Cuellar, Mat'ias" = "Orellana, Matias",
  "Ossand'on Lira, Manuel" = "Ossandon, Manuel",
  "Oyarz'un Solis, Mar'ia Jos'e" = "Oyarzun, Maria Jose",
  "P'erez Espina, Alejandra" = "Perez, Alejandra",
  "Pinto Solari, Malucha" = "Pinto, Malucha",
  "Politzer Kerekes, Patricia" = "Politzer, Patricia",
  "Portilla Barrios, Ericka" = "Portilla, Ericka",
  "Pustilnick Arditi, Tammy" = "Pustilnick, Tammy",
  "Quinteros C'aceres, Mar'ia Elisa" = "Quinteros, Maria Elisa",
  "Rebolledo Aguirre, B'arbara" = "Rebolledo, Barbara",
  "Reyes Painequeo, Mar'ia Ramona" = "Reyes, Maria Ramona",
  "Rivera Bigas, Pollyana" = "Rivera, Pollyana",
  "Rivera Iribarren, Mar'ia Magdalena" = "Rivera, Maria Magdalena",
  "Roa Cadin, Giovanna" = "Roa, Giovanna",
  "Royo Letelier, Manuela" = "Royo, Manuela",
  "S'anchez Mu~noz, Beatriz" = "Sanchez, Beatriz", # Note: ~n -> n
  "Salda~na Mu~noz, Alvin" = "Saldana, Alvin", # Note: ~n -> n
  "Salinas Manfredini, Fernando" = "Salinas, Fernando",
  "San Juan Standen, Constanza" = "San Juan, Constanza",
  "Schonhaut Soto, Constanza" = "Schonhaut, Constanza",
  "Sep'ulveda Hales, B'arbara" = "Sepulveda, Barbara",
  "Sep'ulveda Sep'ulveda, Carolina" = "Sepulveda, Carolina",
  "Serey Jim'enez, Mariela" = "Serey, Mariela",
  "Silva Mora, Luciano" = "Silva, Luciano",
  "Squella Narducci, Agust'in" = "Squella, Agustin",
  "Stingo Camus, Daniel" = "Stingo, Daniel",
  "Tepper Kolossa, Mar'ia Ang'elica" = "Tepper, Maria Angelica",
  "Tirado Soto, Fernando" = "Tirado, Fernando",
  "Toloza Fern'andez, Pablo" = "Toloza, Pablo",
  "Ubilla P'erez, Mar'ia Cecilia" = "Ubilla, Maria Cecilia",
  "Uribe Araya, C'esar" = "Uribe, Cesar",
  "Urrutia Herrera, Tatiana" = "Urrutia, Tatiana",
  "Valenzuela Maass, C'esar" = "Valenzuela, Cesar",
  "Valenzuela Rio, Paulina" = "Valenzuela, Paulina",
  "Vallejos D'avila, Loreto" = "Vallejos, Loreto",
  "Vargas L'opez, Margarita" = "Vargas, Margarita",
  "Vargas Vidal, Mario" = "Vargas, Mario",
  "Vega Campusano, Roberto" = "Vega, Roberto",
  "Vel'asquez N'u~nez, Hern'an" = "Velasquez, Hernan", # Note: ~n -> n
  "Veloso Mu~noz, Paulina" = "Veloso, Paulina", # Note: ~n -> n
  "Vergara Riquelme, Lisette" = "Vergara, Lisette",
  "Vidal Hern'andez, Loreto" = "Vidal, Rossana", # *SPECIAL CASE*: Loreto Vidal vs Rossana Vidal.
  "Videla Osorio, Carolina" = "Videla, Carolina",
  "Viera 'Alvarez, Christian" = "Viera, Christian", # Note: 'Alvarez
  "Vilches Fuenzalida, Carolina" = "Vilches, Carolina",
  "Villena Narbona, Ingrid" = "Villena, Ingrid",
  "Woldarsky Gonz'alez, Manuel" = "Woldarsky, Manuel",
  "Z'arate Z'arate, Camila" = "Zarate, Camila",
  "Z'u~niga Jory, Luis Arturo" = "Zuniga, Luis Arturo"
)

# Verify all names from names_t_test_results are in the dictionary keys
if (!all(unique(t_test_results$legislador) %in% names(name_mapping_dictionary))) {
  stop("Error: Not all names from names_t_test_results are in the dictionary keys!")
}

# Verify all mapped values are in names_point_wnom_dt (optional, but good check)
if (!all(name_mapping_dictionary %in% unique(point_wnom_dt$legislador))) {
  warning("Warning: Some mapped names are not present in the original names_point_wnom_dt vector. This might indicate a typo or an unexpected name.")
  # print(name_mapping_dictionary[!name_mapping_dictionary %in% names_point_wnom_dt]) # to see which ones
}

# Add the new column with matched names
t_test_results$legislador <- name_mapping_dictionary[t_test_results$legislador]

# Check for any NAs which would indicate a missing mapping for a name present in t_test_results$legislador
unique(t_test_results$legislador)
unique(point_wnom_dt$legislador)

if(any(is.na(t_test_results$legislador))) {
  warning("Some names in t_test_results$legislador were not found in the dictionary and resulted in NA.")
  print("Names that resulted in NA:")
  print(unique(t_test_results$legislador[is.na(t_test_results$legislador)]))
}

# fin CAMBIAR NOMBRES EN t_test_results ----

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
