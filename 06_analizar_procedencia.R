library(jsonlite)
library(purrr)
library(dplyr)
library(tidyr)
library(tidytext) # Aunque no usemos unnest_sentences, es útil para stop_words
library(text2vec)
library(stringr)
library(parallel)
library(doParallel)

# --- 1. Configuración ---
cat("--- Configuración del Proceso ---\n")

# Ruta al JSON del borrador final estructurado
BORRADOR_JSON_PATH <- "patrocinantes_identificacion/propuesta_borrador_const_estructurado.json"

# Ruta a la carpeta que contiene los archivos JSON de las iniciativas
INICIATIVAS_JSON_FOLDER <- "patrocinantes_identificacion/"
# Patrón para encontrar los archivos de iniciativas
INICIATIVAS_PATTERN <- "api_extracted_\\d+_\\d+_corrected_4\\.json$"

cat(paste0("Ruta del Borrador JSON: ", BORRADOR_JSON_PATH, "\n"))
cat(paste0("Carpeta de Iniciativas JSON: ", INICIATIVAS_JSON_FOLDER, "\n"))
cat("Estrategia de selección: Top 10 mejores coincidencias por oración.\n")

# --- 2. Carga y Tokenización de Datos ---
cat("\n--- Paso 1: Carga y Tokenización de Datos ---\n")

# --- 2.1 Cargar y Procesar el Borrador Final (JSON) ---
cat("Cargando y procesando el borrador final...\n")
if (!file.exists(BORRADOR_JSON_PATH)) {
  stop("El archivo JSON del borrador no fue encontrado en la ruta especificada.")
}

borrador_df <- fromJSON(BORRADOR_JSON_PATH) %>% as_tibble()

oraciones_borrador_df <- borrador_df %>%
  select(id_articulo_borrador, texto_articulo_limpio) %>%
  # Usar separate_rows para dividir por puntos. El patrón busca un punto seguido de espacio o al final de la cadena.
  separate_rows(texto_articulo_limpio, sep = "\\.\\s*") %>%
  rename(oracion = texto_articulo_limpio) %>%
  filter(str_length(str_trim(oracion)) > 10) %>% # Filtrar oraciones muy cortas
  mutate(
    id_oracion_borrador = row_number(),
    # Texto limpio para el análisis de similitud
    oracion_limpia = str_to_lower(oracion) %>% str_remove_all("[[:punct:]]") %>% str_squish()
  )

cat(paste0("-> Se extrajeron ", nrow(oraciones_borrador_df), " oraciones de ", n_distinct(oraciones_borrador_df$id_articulo_borrador), " artículos del borrador.\n"))

# --- 2.2 Cargar y Procesar las Iniciativas (JSONs) ---
cat("Cargando y procesando las iniciativas...\n")

json_files <- list.files(path = INICIATIVAS_JSON_FOLDER, pattern = INICIATIVAS_PATTERN, full.names = TRUE)
if (length(json_files) == 0) {
  stop("No se encontraron archivos JSON de iniciativas con el patrón especificado.")
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
  separate_rows(propuesta_norma, sep = "\\.\\s*") %>%
  rename(oracion = propuesta_norma) %>%
  filter(str_length(str_trim(oracion)) > 10) %>%
  mutate(
    id_oracion_iniciativa = row_number(),
    oracion_limpia = str_to_lower(oracion) %>% str_remove_all("[[:punct:]]") %>% str_squish()
  )

cat(paste0("-> Se extrajeron ", nrow(oraciones_iniciativas_df), " oraciones de ", nrow(todas_iniciativas_df), " iniciativas.\n"))

# --- 3. Cálculo de Similitud con TF-IDF ---
cat("\n--- Paso 2: Cálculo de Similitud con TF-IDF ---\n")
cat("Preparando corpus y calculando la matriz de similitud del coseno...\n")

corpus_oraciones <- c(oraciones_borrador_df$oracion_limpia, oraciones_iniciativas_df$oracion_limpia)
tokens <- space_tokenizer(corpus_oraciones)
vocab <- create_vocabulary(itoken(tokens), stopwords = stop_words$word[stop_words$lexicon == "snowball"])
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(itoken(tokens), vectorizer)
tfidf_model <- TfIdf$new()
dtm_tfidf <- tfidf_model$fit_transform(dtm)

n_borrador <- nrow(oraciones_borrador_df)
dtm_borrador <- dtm_tfidf[1:n_borrador, ]
dtm_iniciativas <- dtm_tfidf[(n_borrador + 1):nrow(dtm_tfidf), ]

similarity_matrix <- sim2(x = dtm_borrador, y = dtm_iniciativas, method = "cosine", norm = "l2")
cat("-> Matriz de similitud calculada.\n")

# --- 4. Encontrar las Top 10 Coincidencias (EN PARALELO) ---
cat("\n--- Paso 3: Encontrando las Top 10 Coincidencias (en Paralelo) ---\n")
cat("Configurando cluster de 8 núcleos (FORK)...\n")

# Configurar y registrar el cluster paralelo
cl <- makeCluster(8, type = "FORK")
registerDoParallel(cl)

cat("Iniciando búsqueda en paralelo...\n")
start_time <- Sys.time()

# Usar foreach para iterar en paralelo sobre cada oración del borrador
# El resultado se combinará en un único data frame
top_10_matches <- foreach(i = 1:n_borrador, .combine = 'rbind', .packages = c('dplyr')) %dopar% {
  # Tomar la fila de similitudes para la oración i-ésima del borrador
  sim_vector <- similarity_matrix[i, ]
  
  # Ordenar las similitudes de mayor a menor y obtener los índices
  top_indices <- order(sim_vector, decreasing = TRUE)[1:10]
  
  # Obtener los valores de similitud para esos índices
  top_similarities <- sim_vector[top_indices]
  
  # Crear un tibble con los resultados para esta oración del borrador
  tibble(
    id_oracion_borrador = i,
    rank = 1:10, # Rango de la coincidencia (1 a 10)
    id_oracion_iniciativa_match = top_indices,
    similitud = top_similarities
  )
}

end_time <- Sys.time()
execution_time_parallel <- end_time - start_time
cat(paste0("-> Búsqueda en paralelo completada en: ", round(execution_time_parallel, 2), " segundos.\n"))

# Detener el cluster
stopCluster(cl)

# --- 5. Unir Resultados y Calcular Contribuciones ---
cat("\n--- Paso 4: Agregando Resultados y Calculando Contribuciones ---\n")

# Unir con la información de las oraciones del borrador
full_results_df <- top_10_matches %>%
  left_join(oraciones_borrador_df %>% select(id_oracion_borrador, id_articulo_borrador, oracion_borrador = oracion), 
            by = "id_oracion_borrador") %>%
  # Unir con la información de las oraciones de las iniciativas
  left_join(oraciones_iniciativas_df %>% select(id_oracion_iniciativa_match = id_oracion_iniciativa, 
                                                origen_iniciativa_pdf = filename, 
                                                oracion_iniciativa = oracion), 
            by = "id_oracion_iniciativa_match")

# Filtrar para obtener solo la MEJOR coincidencia para cada oración del borrador
best_match_per_oracion <- full_results_df %>%
  filter(rank == 1)

# Calcular Contribución(N, M)
total_oraciones_por_articulo <- oraciones_borrador_df %>%
  count(id_articulo_borrador, name = "total_oraciones_articulo")

contribuciones_df <- best_match_per_oracion %>%
  group_by(id_articulo_borrador, origen_iniciativa_pdf) %>%
  summarise(oraciones_aportadas = n(), .groups = 'drop') %>%
  left_join(total_oraciones_por_articulo, by = "id_articulo_borrador") %>%
  mutate(contribucion_pct = oraciones_aportadas / total_oraciones_articulo) %>%
  select(id_articulo_borrador, origen_iniciativa_pdf, contribucion_pct, oraciones_aportadas, total_oraciones_articulo) %>%
  arrange(id_articulo_borrador, desc(contribucion_pct))

# --- 6. Mostrar Resultados ---
cat("\n--- Resultados Finales ---\n")

cat("\n--- Ejemplo de las Mejores 3 Coincidencias para la Primera Oración del Borrador ---\n")
print(
  full_results_df %>%
    filter(id_oracion_borrador == 1, rank <= 3) %>%
    select(rank, similitud, oracion_borrador, origen_iniciativa_pdf, oracion_iniciativa)
)

cat("\n--- Iniciativas con Mayor Contribución al Artículo 1 del Borrador ---\n")
print(
  contribuciones_df %>%
    filter(id_articulo_borrador == 1) %>%
    arrange(desc(contribucion_pct))
)

cat("\n--- Artículos con Mayor Contribución de la Iniciativa '1001-...' ---\n")
# Busca una iniciativa que exista en tus datos para probar
ejemplo_iniciativa <- contribuciones_df$origen_iniciativa_pdf[1]
print(
  contribuciones_df %>%
    filter(str_detect(origen_iniciativa_pdf, str_extract(ejemplo_iniciativa, "^\\d+"))) %>%
    arrange(desc(contribucion_pct)) %>%
    head(10)
)
