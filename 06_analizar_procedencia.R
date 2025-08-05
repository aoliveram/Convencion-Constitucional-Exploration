# --- 0. Instalación y Carga de Paquetes ---

install.packages("pdftools")
install.packages("jsonlite")
install.packages("text2vec")
install.packages("cli")
install.packages("tidytext")

# Cargar las librerías necesarias
library(pdftools)
library(jsonlite)
library(purrr)
library(dplyr)
library(tidyr)
library(tidytext)
library(text2vec)
library(stringr)
library(cli) # Para mostrar mensajes de progreso más claros

# --- 1. Configuración ---
cli_h1("Configuración del Proceso")

# Ruta al PDF del borrador final
BORRADOR_PDF_PATH <- "patrocinantes_identificacion/PROPUESTA-DE-BORRADOR-CONSTITUCIONAL-14-05-22.pdf" # <-- ¡MODIFICA ESTA RUTA!

# Ruta a la carpeta que contiene los archivos JSON de las iniciativas
INICIATIVAS_JSON_FOLDER <- "patrocinantes_identificacion/" # <-- ¡MODIFICA ESTA RUTA!

# Umbral de similitud: solo se considerarán coincidencias por encima de este valor (0 a 1)
SIMILARITY_THRESHOLD <- 0.65 # Puedes ajustar este valor (0.65 es un buen punto de partida)

cli_alert_info("Ruta del Borrador PDF: {BORRADOR_PDF_PATH}")
cli_alert_info("Carpeta de Iniciativas JSON: {INICIATIVAS_JSON_FOLDER}")
cli_alert_info("Umbral de Similitud: {SIMILARITY_THRESHOLD}")

# --- 2. Carga y Pre-procesamiento de Datos ---
cli_h1("Paso 1: Carga y Pre-procesamiento de Datos")

# --- 2.1 Cargar y Estructurar el Borrador Final (PDF) ---
cli_progress_step("Cargando y estructurando el borrador final (PDF)...")

if (!file.exists(BORRADOR_PDF_PATH)) {
  stop("El archivo PDF del borrador no fue encontrado en la ruta especificada.")
}

# Leer el texto de todas las páginas y unirlas
texto_completo_borrador <- pdf_text(BORRADOR_PDF_PATH) %>%
  paste(collapse = " \n ")

# Limpieza inicial
texto_completo_borrador <- texto_completo_borrador %>%
  str_replace_all("-\n", "") %>% # Unir palabras cortadas por saltos de línea
  str_replace_all("\n", " ") %>% # Reemplazar otros saltos de línea con espacios
  str_squish() # Eliminar espacios múltiples

# Usar expresiones regulares para dividir el texto en artículos
articulos_borrador_df <- tibble(texto_completo = texto_completo_borrador) %>%
  mutate(
    # Dividir el texto usando un patrón que busca el inicio de un artículo
    # CORRECCIÓN: Se eliminó un paréntesis extra ')' al final del patrón regex.
    articulos = str_split(texto_completo, "(?=(?:\\d{1,3}\\.\\-)?\\s*Artículo\\s+[\\d°ºA-Za-z]+\\.?-)"),
  ) %>%
  unnest(articulos) %>%
  select(texto_articulo = articulos) %>%
  filter(str_detect(texto_articulo, regex("^\\s*(?:\\d{1,3}\\.\\-)?\\s*Artículo", ignore_case = TRUE))) %>%
  mutate(
    # Extraer el número o identificador del artículo
    id_articulo = str_extract(texto_articulo, regex("Artículo\\s+[\\d°ºA-Za-z]+", ignore_case = TRUE)),
    # Limpiar el texto del artículo
    # CORRECCIÓN: La expresión regular aquí debe coincidir con la de arriba para una limpieza correcta.
    # El patrón original en str_remove estaba bien. Lo mantendré consistente.
    texto_articulo_limpio = str_remove(texto_articulo, regex("^(?:\\d{1,3}\\.\\-)?\\s*Artículo\\s+[\\d°ºA-Za-z]+\\.?-\\s*", ignore_case = TRUE)) %>% str_trim()
  )

# Tokenizar el borrador en oraciones
oraciones_borrador_df <- articulos_borrador_df %>%
  select(id_articulo, texto_articulo_limpio) %>%
  unnest_sentences(oracion, texto_articulo_limpio, to_lower = FALSE) %>% # Mantener mayúsculas por ahora
  filter(str_length(oracion) > 10) %>% # Filtrar oraciones muy cortas/ruido
  mutate(
    id_oracion_borrador = row_number(), # Crear un ID único para cada oración del borrador
    oracion_limpia = str_to_lower(oracion) %>% str_remove_all("[[:punct:]]") # Limpiar para comparación
  )

cli_progress_done()
cli_alert_success("{nrow(oraciones_borrador_df)} oraciones extraídas del borrador final.")


# --- 2.2 Cargar y Estructurar las Iniciativas (JSONs) ---
cli_progress_step("Cargando y estructurando las iniciativas (JSONs)...")

json_files <- list.files(path = INICIATIVAS_JSON_FOLDER, pattern = "api_extracted_.*\\.json", full.names = TRUE)

if (length(json_files) == 0) {
  stop("No se encontraron archivos JSON de iniciativas en la carpeta especificada.")
}

# Cargar todos los JSON en un único data frame
todas_iniciativas_df <- map_dfr(json_files, ~{
  data <- read_json(.x)
  # Procesar cada archivo, manejando posibles errores o campos nulos
  map_dfr(data, .id = "filename", ~{
    # Asegurarse de que propuesta_norma no sea NULL y sea un string
    propuesta <- .x$propuesta_norma
    if (is.null(propuesta) || !is.character(propuesta)) {
      propuesta <- "" # Asignar un string vacío si es nulo
    }
    tibble(propuesta_norma = propuesta)
  })
}) %>%
  filter(propuesta_norma != "") # Filtrar iniciativas que no tenían propuesta de norma

# Tokenizar las iniciativas en oraciones
oraciones_iniciativas_df <- todas_iniciativas_df %>%
  unnest_sentences(oracion, propuesta_norma, to_lower = FALSE) %>%
  filter(str_length(oracion) > 10) %>% # Filtrar ruido
  mutate(
    id_oracion_iniciativa = row_number(), # Crear un ID único para cada oración de iniciativa
    oracion_limpia = str_to_lower(oracion) %>% str_remove_all("[[:punct:]]") # Limpiar para comparación
  )

cli_progress_done()
cli_alert_success("{nrow(oraciones_iniciativas_df)} oraciones extraídas de {nrow(todas_iniciativas_df)} iniciativas.")


# --- 3. Cálculo de Similitud con TF-IDF ---
cli_h1("Paso 2: Cálculo de Similitud con TF-IDF")
cli_progress_step("Preparando corpus y calculando similitud del coseno...")

# Combinar todas las oraciones limpias en un único vector (corpus)
corpus_oraciones <- c(oraciones_borrador_df$oracion_limpia, oraciones_iniciativas_df$oracion_limpia)

# Crear el tokenizador y el vocabulario usando text2vec
# Se eliminan las stop words en español para mejorar la relevancia
tokens <- space_tokenizer(corpus_oraciones)
vocab <- create_vocabulary(itoken(tokens), stopwords = tidytext::stop_words$word[tidytext::stop_words$lexicon == "snowball"])

# Crear la Matriz Documento-Término (DTM)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(itoken(tokens), vectorizer)

# Aplicar el modelo TF-IDF
tfidf_model <- TfIdf$new()
dtm_tfidf <- tfidf_model$fit_transform(dtm)

# Dividir la matriz TF-IDF en dos: una para el borrador y otra para las iniciativas
n_borrador <- nrow(oraciones_borrador_df)
dtm_borrador <- dtm_tfidf[1:n_borrador, ]
dtm_iniciativas <- dtm_tfidf[(n_borrador + 1):nrow(dtm_tfidf), ]

# Calcular la matriz de similitud del coseno
# Esto puede tardar unos segundos si hay muchas oraciones
similarity_matrix <- sim2(x = dtm_borrador, y = dtm_iniciativas, method = "cosine", norm = "l2")

cli_progress_done()
cli_alert_success("Matriz de similitud calculada.")


# --- 4. Encontrar las Mejores Coincidencias ---
cli_h1("Paso 3: Encontrando las Mejores Coincidencias")
cli_progress_step("Identificando la mejor coincidencia para cada oración del borrador...")

# Crear un data frame con los resultados
# Inicializar columnas para guardar los resultados
best_match_index <- rep(NA, n_borrador)
best_match_similarity <- rep(NA, n_borrador)

# Iterar sobre cada fila de la matriz de similitud (cada oración del borrador)
for (i in 1:n_borrador) {
  # Encontrar el índice de la máxima similitud en la fila i
  max_sim_idx <- which.max(similarity_matrix[i, ])
  
  # Si hay una coincidencia (no está vacío)
  if (length(max_sim_idx) > 0) {
    max_sim_value <- similarity_matrix[i, max_sim_idx]
    
    # Guardar solo si la similitud supera el umbral
    if (max_sim_value >= SIMILARITY_THRESHOLD) {
      best_match_index[i] <- max_sim_idx
      best_match_similarity[i] <- max_sim_value
    }
  }
}

# Añadir los resultados al data frame de oraciones del borrador
oraciones_borrador_df$id_oracion_iniciativa_match <- best_match_index
oraciones_borrador_df$similitud <- best_match_similarity

# Unir con la información de las iniciativas para obtener el texto original y el nombre del archivo
resultados_finales_df <- oraciones_borrador_df %>%
  # Mantener solo las que tuvieron una coincidencia por encima del umbral
  filter(!is.na(id_oracion_iniciativa_match)) %>%
  # Unir con el data frame de oraciones de las iniciativas
  left_join(
    oraciones_iniciativas_df %>% select(id_oracion_iniciativa_match = id_oracion_iniciativa,
                                        filename,
                                        oracion_iniciativa_original = oracion),
    by = "id_oracion_iniciativa_match"
  ) %>%
  # Ordenar y seleccionar columnas finales
  select(
    id_articulo_borrador = id_articulo,
    oracion_borrador = oracion,
    similitud,
    origen_iniciativa_pdf = filename,
    oracion_iniciativa_original
  ) %>%
  arrange(id_articulo_borrador)

cli_progress_done()
cli_alert_success("Proceso de coincidencia completado.")


# --- 5. Mostrar Resultados ---
cli_h1("Resultados Finales")

if (nrow(resultados_finales_df) > 0) {
  cli_alert_info("Mostrando las primeras 20 coincidencias encontradas:")
  print(head(resultados_finales_df, 20), width = 150) # Ajustar ancho de impresión
  
  # Resumen por artículo
  resumen_por_articulo <- resultados_finales_df %>%
    group_by(id_articulo_borrador) %>%
    summarise(
      oraciones_coincidentes = n(),
      iniciativas_principales = paste(names(sort(table(origen_iniciativa_pdf), decreasing = TRUE)[1:2]), collapse = ", "),
      similitud_promedio = mean(similitud)
    )
  
  cli_h2("Resumen de Contribuciones por Artículo")
  print(resumen_por_articulo)
  
  # Resumen por iniciativa
  resumen_por_iniciativa <- resultados_finales_df %>%
    group_by(origen_iniciativa_pdf) %>%
    summarise(
      oraciones_aportadas = n()
    ) %>%
    arrange(desc(oraciones_aportadas))
  
  cli_h2("Resumen de Aportes por Iniciativa (Top 10)")
  print(head(resumen_por_iniciativa, 10))
  
} else {
  cli_alert_warning("No se encontraron coincidencias por encima del umbral de similitud ({SIMILARITY_THRESHOLD}).")
  cli_alert_info("Intenta bajar el umbral (ej. 0.5) o revisa el pre-procesamiento de texto.")
}