library(jsonlite)
library(purrr)
library(dplyr)
library(tidyr)
library(tidytext) # útil para stop_words
library(text2vec)
library(stringr)
library(parallel)
library(doParallel)
library(stringr)

# --- 1. Configuración ---
cat("--- Configuración del Proceso ---\n")

# borrador final estructurado
BORRADOR_JSON_PATH <- "patrocinantes_identificacion/propuesta_borrador_const_estructurado.json"

# archivos JSON de las iniciativas
INICIATIVAS_JSON_FOLDER <- "patrocinantes_identificacion/"
INICIATIVAS_PATTERN <- "api_extracted_\\d+_\\d+_corrected_4\\.json$"

# --- 2. Carga y Tokenización de Datos ---

# --- 2.1 Borrador Constitucional (JSON) ---
if (!file.exists(BORRADOR_JSON_PATH)) {
  stop("El archivo JSON del borrador no fue encontrado en la ruta especificada.")
}

borrador_df <- fromJSON(BORRADOR_JSON_PATH) %>% as_tibble()

oraciones_borrador_df <- borrador_df %>%
  select(id_articulo_borrador, texto_articulo_limpio) %>%
  # separate_rows busca punto seguido de espacio o al final de la cadena.
  separate_rows(texto_articulo_limpio, sep = "\\.\\s*") %>%
  rename(oracion = texto_articulo_limpio) %>%
  filter(str_length(str_trim(oracion)) > 10) %>% # filtramos oraciones muy cortas
  mutate(
    id_oracion_borrador = row_number(),
    # Texto limpio para el análisis de similitud
    oracion_limpia = str_to_lower(oracion) %>% str_remove_all("[[:punct:]]") %>% str_squish()
  )

cat(paste0("-> Se extrajeron ", nrow(oraciones_borrador_df), " oraciones de ", n_distinct(oraciones_borrador_df$id_articulo_borrador), " artículos del borrador.\n"))

oraciones_borrador_dist <- oraciones_borrador_df %>%
  mutate(
    n_palabras = str_count(oracion_limpia, "\\S+")
  ) %>%
  count(n_palabras, name = "frecuencia") %>%
  arrange(n_palabras)

print(oraciones_borrador_dist, n=50)

# --- 2.2 Iniciativas Normas (JSONs) ---

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

# Tokenizar las iniciativas en oraciones
oraciones_iniciativas_df <- todas_iniciativas_df %>%
  separate_rows(propuesta_norma, sep = "\\.\\s*") %>%
  rename(oracion = propuesta_norma) %>%
  mutate(
    # versión limpia temporal para aplicar los filtros
    oracion_temp_limpia = str_to_lower(oracion)
  ) %>%
  # Filtro 1: Eliminar oraciones que contengan la palabra "artículo"
  filter(!str_detect(oracion_temp_limpia, "\\bartículo\\b|\\barticulo\\b")) %>%
  # Filtro 2: Eliminar oraciones con menos de 4 palabras
  mutate(
    n_palabras = str_count(oracion_temp_limpia, "\\S+")
  ) %>%
  filter(n_palabras >= 4) %>%
  mutate(
    id_oracion_iniciativa = row_number(),
    # añadimos oracion_limpia
    oracion_limpia = str_remove_all(oracion_temp_limpia, "[[:punct:]]") %>% str_squish()
  ) %>%
  # eliminamos las temporales
  select(filename, oracion, id_oracion_iniciativa, oracion_limpia)

cat(paste0("-> Se extrajeron ", nrow(oraciones_iniciativas_df), " oraciones VÁLIDAS (después del filtrado) de ", nrow(todas_iniciativas_df), " iniciativas.\n"))

oraciones_iniciativas_dist <- oraciones_iniciativas_df %>%
  mutate(
    n_palabras = str_count(oracion_limpia, "\\S+")
  ) %>%
  count(n_palabras, name = "frecuencia") %>%
  arrange(n_palabras)

print(oraciones_iniciativas_dist, n=50)

# --- 3. Cálculo de Similitud con TF-IDF ---

# cargamos la lista de stop words en español
spanish_stopwords <- stop_words %>% filter(lexicon == "snowball") %>% pull(word)

# Crear el iterador de tokens
it_full_corpus <- itoken(oraciones_iniciativas_df$oracion_limpia, 
                         preprocessor = str_to_lower, 
                         tokenizer = space_tokenizer, 
                         progressbar = FALSE)

# vocabulario inicial
vocab <- create_vocabulary(it_full_corpus, stopwords = spanish_stopwords, ngram = c(1L, 1L))
# Filtro 1: Eliminar palabras que aparecen solo una vez
vocab <- prune_vocabulary(vocab, term_count_min = 2)
# Filtro 2: Eliminar tokens que no son palabras (contienen números o no son letras)
vocab <- vocab %>% 
  filter(str_detect(term, "^[a-záéíóúüñ]+$"))

vectorizer <- vocab_vectorizer(vocab)

# Crear la DTM para AMBOS conjuntos de datos usando el MISMO vectorizador
it_borrador <- itoken(oraciones_borrador_df$oracion_limpia, tokenizer = space_tokenizer, progressbar = FALSE)
it_iniciativas <- itoken(oraciones_iniciativas_df$oracion_limpia, tokenizer = space_tokenizer, progressbar = FALSE)
dtm_borrador <- create_dtm(it_borrador, vectorizer)
dtm_iniciativas <- create_dtm(it_iniciativas, vectorizer)

# Unir las DTMs en una sola matriz para el entrenamiento
full_dtm <- rbind(dtm_borrador, dtm_iniciativas)

# Crear el modelo TF-IDF y aplicarlo a la matriz completa
# fit_transform entrena el modelo IDF con full_dtm y la transforma, todo en un paso.
tfidf_model <- TfIdf$new()
full_dtm_tfidf <- tfidf_model$fit_transform(full_dtm)

# dividimos en las partes Borrador e Iniciativas
n_borrador <- nrow(oraciones_borrador_df)
dtm_borrador_tfidf <- full_dtm_tfidf[1:n_borrador, ]
dtm_iniciativas_tfidf <- full_dtm_tfidf[(n_borrador + 1):nrow(full_dtm_tfidf), ]

# Calculamos la matriz de similitud del coseno
similarity_matrix <- sim2(x = dtm_borrador_tfidf, y = dtm_iniciativas_tfidf, method = "cosine", norm = "l2")

# --- 4. Encontrar las Top 10 Coincidencias ---

cl <- makeCluster(8, type = "FORK")
registerDoParallel(cl)

start_time <- Sys.time()

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

stopCluster(cl)

# --- 5. Unir Resultados y Calcular Contribuciones ---

full_results_df <- top_10_matches %>%
  left_join(oraciones_borrador_df %>% select(id_oracion_borrador, id_articulo_borrador, oracion_borrador = oracion), 
            by = "id_oracion_borrador") %>%
  left_join(oraciones_iniciativas_df %>% select(id_oracion_iniciativa_match = id_oracion_iniciativa, 
                                                origen_iniciativa_pdf = filename, 
                                                oracion_iniciativa = oracion), 
            by = "id_oracion_iniciativa_match")

best_match_per_oracion <- full_results_df %>%
  filter(rank == 1)

# Calcular el número total de oraciones por artículo del borrador
total_oraciones_por_articulo <- oraciones_borrador_df %>%
  count(id_articulo_borrador, name = "total_oraciones_articulo")

# Contribución(N, M) 
# Nos preguntamos aquí: ¿qué tanto contribuye la iniciativa M al artículo N del borrador??

# Paso 1: Calcular las métricas agregadas (promedio, conteo) por artículo e iniciativa
metricas_agregadas <- best_match_per_oracion %>%
  group_by(id_articulo_borrador, origen_iniciativa_pdf) %>%
  summarise(
    # La nueva columna con el promedio, con el nombre que sugeriste
    contrib_media_iniciat = mean(similitud, na.rm = TRUE),
    # El conteo de oraciones aportadas
    oraciones_aportadas = n(),
    .groups = 'drop'
  )

# Paso 2: Unir métricas agregadas al data frame de coincidencias individuales
contribuciones_df_detallado <- best_match_per_oracion %>%
  # Unir con las métricas agregadas que acabamos de calcular
  left_join(metricas_agregadas, by = c("id_articulo_borrador", "origen_iniciativa_pdf")) %>%
  # Unir con el conteo total de oraciones por artículo
  left_join(total_oraciones_por_articulo, by = "id_articulo_borrador") %>%
  # Calcular el porcentaje de contribución
  mutate(contribucion_pct = (oraciones_aportadas / total_oraciones_articulo) * 100) %>%
  # Seleccionar y reordenar las columnas finales
  select(
    id_articulo_borrador,
    id_oracion_borrador,
    origen_iniciativa_pdf,
    similitud, 
    contrib_media_iniciat, 
    contribucion_pct,
    oraciones_aportadas,
    total_oraciones_articulo,
    oracion_borrador, # Incluir los textos para contexto
    oracion_iniciativa
  ) %>%
  # Ordenar por artículo y luego por oración
  arrange(id_articulo_borrador, id_oracion_borrador)

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

# --- 7. Guardar objetos para usar en Python ---
cat("\n--- Guardando objetos para análisis en Python ---\n")

saveRDS(oraciones_borrador_df, file = "scripts - files/analizar_procedencia_borrador/oraciones_borrador_df.rds")
saveRDS(oraciones_iniciativas_df, file = "scripts - files/analizar_procedencia_borrador/oraciones_iniciativas_df.rds")
readr::write_csv(oraciones_iniciativas_df, "scripts - files/analizar_procedencia_borrador/oraciones_iniciativas_df.csv")
saveRDS(top_10_matches, file = "scripts - files/analizar_procedencia_borrador/top10_tfidf_matches.rds")