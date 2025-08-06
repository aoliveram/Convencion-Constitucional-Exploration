# --- 0. Instalación y Carga de Paquetes ---

# Asegúrate de tener todos los paquetes instalados. Si no, ejecútalo una vez:
# install.packages(c("pdftools", "jsonlite", "purrr", "dplyr", "tidyr", "tidytext", "text2vec", "stringr", "parallel", "doParallel"))

# Cargar las librerías necesarias
library(pdftools)
library(jsonlite)
library(purrr)
library(dplyr)
library(tidyr)
library(tidytext)
library(text2vec)
library(stringr)
library(parallel) # Para makeCluster
library(doParallel) # Para registrar el backend paralelo

# --- 1. Configuración ---
cat("--- Configuración del Proceso ---\n")

# Ruta al PDF del borrador final
BORRADOR_PDF_PATH <- "patrocinantes_identificacion/PROPUESTA-DE-BORRADOR-CONSTITUCIONAL-14-05-22.pdf" # <-- ¡MODIFICA ESTA RUTA!

# Ruta a la carpeta que contiene los archivos JSON de las iniciativas
INICIATIVAS_JSON_FOLDER <- "patrocinantes_identificacion/" # <-- ¡MODIFICA ESTA RUTA!

# Umbral de similitud: solo se considerarán coincidencias por encima de este valor (0 a 1)
SIMILARITY_THRESHOLD <- 0.65 # Puedes ajustar este valor

cat(paste0("Ruta del Borrador PDF: ", BORRADOR_PDF_PATH, "\n"))
cat(paste0("Carpeta de Iniciativas JSON: ", INICIATIVAS_JSON_FOLDER, "\n"))
cat(paste0("Umbral de Similitud: ", SIMILARITY_THRESHOLD, "\n"))

# --- 2. Carga y Pre-procesamiento de Datos ---
cat("\n--- Paso 1: Carga y Pre-procesamiento de Datos ---\n")

# --- 2.1 Cargar y Estructurar el Borrador Final (PDF) ---
cat("Cargando y estructurando el borrador final (PDF)...\n")

if (!file.exists(BORRADOR_PDF_PATH)) {
  stop("El archivo PDF del borrador no fue encontrado en la ruta especificada.")
}

# Leer el texto de todas las páginas y unirlas
texto_completo_borrador <- pdf_text(BORRADOR_PDF_PATH) %>%
  paste(collapse = " \n ")

# Limpieza inicial
texto_completo_borrador <- texto_completo_borrador %>%
  str_replace_all("-\n", "") %>%
  str_replace_all("\n", " ") %>%
  str_squish()

# Grupo 1: (\d{1,3}) -> El número identificador global
# Grupo 2: (Artículo.*?) -> El texto completo del artículo, incluyendo "Artículo X.-"
# Lookahead: (?=...) -> Termina la coincidencia justo antes del siguiente artículo o del fin del texto
extract_pattern <- "(\\d{1,3})\\.\\-\\s*(Artículo.*?)(?=\\d{1,3}\\.\\-|$)"

# Usar str_match_all para encontrar todas las ocurrencias y capturar los grupos
matches <- str_match_all(texto_completo_borrador, extract_pattern)

# str_match_all devuelve una lista con una matriz. La extraemos.
# Columna 1: Coincidencia completa
# Columna 2: Grupo 1 (nuestro ID)
# Columna 3: Grupo 2 (nuestro texto de artículo)
articulos_borrador_df <- as_tibble(matches[[1]][, c(2, 3)], .name_repair = "minimal")
colnames(articulos_borrador_df) <- c("id_articulo_borrador", "texto_articulo")

# Limpiar y convertir el ID a numérico
articulos_borrador_df <- articulos_borrador_df %>%
  mutate(
    id_articulo_borrador = as.numeric(id_articulo_borrador),
    # Limpiar el texto del artículo para quitar el "Artículo X.-" inicial
    texto_articulo_limpio = str_remove(texto_articulo, regex("^Artículo\\s+[\\d°ºA-Za-z]+\\.?-\\s*", ignore_case = TRUE)) %>% str_squish()
  ) %>%
  filter(!is.na(id_articulo_borrador)) # Asegurarse de que no haya IDs fallidos

# Tokenizar el borrador en oraciones
oraciones_borrador_df <- articulos_borrador_df %>%
  select(id_articulo_borrador, texto_articulo_limpio) %>%
  unnest_sentences(oracion, texto_articulo_limpio, to_lower = FALSE) %>%
  filter(str_length(oracion) > 10) %>%
  mutate(
    id_oracion_borrador = row_number(),
    oracion_limpia = str_to_lower(oracion) %>% str_remove_all("[[:punct:]]")
  )

cat(paste0("-> Se extrajeron ", nrow(oraciones_borrador_df), " oraciones de ", n_distinct(oraciones_borrador_df$id_articulo_borrador), " artículos únicos del borrador final.\n"))


# --- 2.2 Cargar y Estructurar las Iniciativas (JSONs) ---
cat("Cargando y estructurando las iniciativas (JSONs)...\n")

json_files <- list.files(path = INICIATIVAS_JSON_FOLDER, pattern = "api_extracted_.*\\.json", full.names = TRUE)
if (length(json_files) == 0) {
  stop("No se encontraron archivos JSON de iniciativas en la carpeta especificada.")
}

todas_iniciativas_df <- map_dfr(json_files, ~{
  data <- read_json(.x)
  map_dfr(data, .id = "filename", ~{
    propuesta <- .x$propuesta_norma
    if (is.null(propuesta) || !is.character(propuesta)) {
      propuesta <- ""
    }
    tibble(propuesta_norma = propuesta)
  })
}) %>%
  filter(propuesta_norma != "")

oraciones_iniciativas_df <- todas_iniciativas_df %>%
  unnest_sentences(oracion, propuesta_norma, to_lower = FALSE) %>%
  filter(str_length(oracion) > 10) %>%
  mutate(
    id_oracion_iniciativa = row_number(),
    oracion_limpia = str_to_lower(oracion) %>% str_remove_all("[[:punct:]]")
  )

cat(paste0("-> Se extrajeron ", nrow(oraciones_iniciativas_df), " oraciones de ", nrow(todas_iniciativas_df), " iniciativas.\n"))


# --- 3. Cálculo de Similitud con TF-IDF ---
cat("\n--- Paso 2: Cálculo de Similitud con TF-IDF ---\n")
cat("Preparando corpus y calculando similitud del coseno...\n")

corpus_oraciones <- c(oraciones_borrador_df$oracion_limpia, oraciones_iniciativas_df$oracion_limpia)
tokens <- space_tokenizer(corpus_oraciones)
vocab <- create_vocabulary(itoken(tokens), stopwords = tidytext::stop_words$word[tidytext::stop_words$lexicon == "snowball"])
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(itoken(tokens), vectorizer)
tfidf_model <- TfIdf$new()
dtm_tfidf <- tfidf_model$fit_transform(dtm)

n_borrador <- nrow(oraciones_borrador_df)
dtm_borrador <- dtm_tfidf[1:n_borrador, ]
dtm_iniciativas <- dtm_tfidf[(n_borrador + 1):nrow(dtm_tfidf), ]

similarity_matrix <- sim2(x = dtm_borrador, y = dtm_iniciativas, method = "cosine", norm = "l2")
cat("-> Matriz de similitud calculada.\n")

# --- 4. Encontrar las Mejores Coincidencias (EN PARALELO) ---
cat("\n--- Paso 3: Encontrando las Mejores Coincidencias (en Paralelo) ---\n")
cat("Configurando cluster de 8 núcleos (FORK)...\n")

# Configurar y registrar el cluster paralelo
cl <- makeCluster(8, type = "FORK")
registerDoParallel(cl)

cat("Iniciando búsqueda en paralelo...\n")
start_time <- Sys.time()

# Usar foreach para iterar en paralelo. El resultado se combinará en una lista.
# .combine='rbind' junta cada resultado (un data frame de una fila) en un único data frame.
# .packages='dplyr' asegura que cada núcleo tenga acceso a las funciones de dplyr.
parallel_results <- foreach(i = 1:n_borrador, .combine = 'rbind', .packages = c('dplyr')) %dopar% {
  # Encontrar el índice y el valor de la máxima similitud en la fila i
  max_sim_idx <- which.max(similarity_matrix[i, ])
  
  if (length(max_sim_idx) > 0) {
    max_sim_value <- similarity_matrix[i, max_sim_idx]
    
    # Si la similitud supera el umbral, devolver una fila con los datos
    if (max_sim_value >= SIMILARITY_THRESHOLD) {
      tibble(
        id_oracion_borrador = i,
        id_oracion_iniciativa_match = max_sim_idx,
        similitud = max_sim_value
      )
    }
  }
}

end_time <- Sys.time()
execution_time_parallel <- end_time - start_time
cat(paste0("-> Búsqueda en paralelo completada en: ", round(execution_time_parallel, 2), " segundos.\n"))

# Detener el cluster
stopCluster(cl)

# Unir los resultados paralelos con los data frames originales
resultados_finales_df <- parallel_results %>%
  left_join(oraciones_borrador_df, by = "id_oracion_borrador") %>%
  left_join(
    oraciones_iniciativas_df %>% select(id_oracion_iniciativa, filename, oracion_iniciativa_original = oracion),
    by = c("id_oracion_iniciativa_match" = "id_oracion_iniciativa")
  ) %>%
  select(
    id_articulo_borrador,
    oracion_borrador = oracion,
    similitud,
    origen_iniciativa_pdf = filename,
    oracion_iniciativa_original
  ) %>%
  arrange(id_articulo_borrador, oracion_borrador)

cat("-> Proceso de coincidencia completado.\n")

# --- 5. Mostrar Resultados ---
cat("\n--- Resultados Finales ---\n")

if (nrow(resultados_finales_df) > 0) {
  cat("Mostrando las primeras 20 coincidencias encontradas:\n")
  print(head(resultados_finales_df, 20), width = 150)
  
  # Resumen por artículo
  resumen_por_articulo <- resultados_finales_df %>%
    group_by(id_articulo_borrador) %>%
    summarise(
      oraciones_coincidentes = n(),
      iniciativas_principales = paste(names(sort(table(origen_iniciativa_pdf), decreasing = TRUE)[1:2]), collapse = ", "),
      similitud_promedio = mean(similitud),
      .groups = 'drop' # Para evitar warnings de agrupación
    )
  
  cat("\n--- Resumen de Contribuciones por Artículo ---\n")
  print(as.data.frame(resumen_por_articulo)) # Imprimir como data.frame para mejor formato
  
  # Resumen por iniciativa
  resumen_por_iniciativa <- resultados_finales_df %>%
    group_by(origen_iniciativa_pdf) %>%
    summarise(
      oraciones_aportadas = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(oraciones_aportadas))
  
  cat("\n--- Resumen de Aportes por Iniciativa (Top 10) ---\n")
  print(head(resumen_por_iniciativa, 10))
  
} else {
  cat(paste0("ADVERTENCIA: No se encontraron coincidencias por encima del umbral de similitud (", SIMILARITY_THRESHOLD, ").\n"))
  cat("Intenta bajar el umbral (ej. 0.5) o revisa el pre-procesamiento de texto.\n")
}