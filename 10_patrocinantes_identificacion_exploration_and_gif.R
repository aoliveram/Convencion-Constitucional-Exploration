library(jsonlite)
library(igraph)
library(dplyr)
library(purrr)
library(lubridate)
library(magick)

# Asegurarse de que R pueda interpretar meses en español
tryCatch(
  Sys.setlocale("LC_TIME", "es_ES.UTF-8"),
  error = function(e) {
    tryCatch(Sys.setlocale("LC_TIME", "Spanish"),
             error = function(e2) warning("No se pudo configurar el locale a español."))
  }
)

# --- 1. Carga y Consolidación de Datos ---
data_folder <- "patrocinantes_identificacion"
json_files <- list.files(path = data_folder,
                         pattern = "api_extracted_\\d+_\\d+_corrected_3\\.json$",
                         full.names = TRUE)

if (length(json_files) == 0) {
  stop("No se encontraron archivos JSON en la carpeta '", data_folder, "'.")
}
cat(paste("Se encontraron", length(json_files), "archivos JSON para procesar.\n"))

process_json_file <- function(file_path) {
  raw_data <- fromJSON(file_path)
  imap_dfr(raw_data, ~{
    doc_id_match <- regexpr("^[0-9]+", .y)
    if (doc_id_match == -1) return(NULL)
    doc_id <- regmatches(.y, doc_id_match)
    firmantes <- .x$firmantes_matched
    if (is.null(firmantes) || length(firmantes) == 0) return(NULL)
    tibble(doc_id = doc_id, fecha_str = .x$fecha, convencional = firmantes)
  })
}

all_initiatives_df <- map_dfr(json_files, process_json_file)
cat(paste("Total de patrocinios individuales procesados:", nrow(all_initiatives_df), "\n"))

# --- 2. Limpieza y Formato de Fechas ---
all_initiatives_df <- all_initiatives_df %>%
  mutate(fecha = dmy(fecha_str, quiet = TRUE)) %>%
  filter(!is.na(fecha))
cat(paste("Total de patrocinios con fecha válida:", nrow(all_initiatives_df), "\n"))

# --- 3. Definición de Bloques Temporales ---
week_blocks <- list(
  list(start_date = ymd("2021-11-10"), end_date = ymd("2021-11-19"), label = "Bloque 1 (10-Nov a 19-Nov)"),
  list(start_date = ymd("2021-11-22"), end_date = ymd("2021-11-28"), label = "Bloque 2 (22-Nov a 28-Nov)"),
  list(start_date = ymd("2021-11-29"), end_date = ymd("2021-12-05"), label = "Bloque 3 (29-Nov a 05-Dic)"),
  list(start_date = ymd("2021-12-06"), end_date = ymd("2021-12-12"), label = "Bloque 4 (06-Dic a 12-Dic)"),
  list(start_date = ymd("2021-12-13"), end_date = ymd("2021-12-19"), label = "Bloque 5 (13-Dic a 19-Dic)"),
  list(start_date = ymd("2021-12-20"), end_date = ymd("2021-12-26"), label = "Bloque 6 (20-Dic a 26-Dic)"),
  list(start_date = ymd("2021-12-27"), end_date = ymd("2022-01-02"), label = "Bloque 7 (27-Dic a 02-Ene)"),
  list(start_date = ymd("2022-01-03"), end_date = ymd("2022-01-09"), label = "Bloque 8 (03-Ene a 09-Ene)"),
  list(start_date = ymd("2022-01-10"), end_date = ymd("2022-01-16"), label = "Bloque 9 (10-Ene a 16-Ene)"),
  list(start_date = ymd("2022-01-17"), end_date = ymd("2022-01-23"), label = "Bloque 10 (17-Ene a 23-Ene)"),
  list(start_date = ymd("2022-01-24"), end_date = ymd("2022-02-01"), label = "Bloque 11 (24-Ene a 01-Feb)")
)


#############################################################################
# PARTE 1: CÁLCULO DEL LAYOUT BASADO EN COMUNIDADES DEL ÚLTIMO PERÍODO
#############################################################################
cat("\n--- PARTE 1: Calculando Layout Maestro basado en Comunidades ---\n")

# A. Crear el grafo de referencia con TODOS los nodos y lazos (proj_conv_full)
edge_list_full <- all_initiatives_df %>% select(doc_id, convencional)
g_bipartite_full <- graph_from_data_frame(edge_list_full, directed = FALSE)
V(g_bipartite_full)$type <- V(g_bipartite_full)$name %in% all_initiatives_df$convencional
proj_conv_full <- bipartite_projection(g_bipartite_full, which = TRUE)

# B. Crear la red específica del último período denso para detectar comunidades
df_layout_base <- all_initiatives_df %>%
  filter(fecha >= ymd("2022-01-28") & fecha <= ymd("2022-02-01"))

# Es posible que en este período específico no haya lazos, si eso pasa, el proj_conv_layout_base
# se creará sin lazos. Necesitamos manejar ese caso.
if (nrow(df_layout_base) > 0) {
  edge_list_layout_base <- df_layout_base %>% select(doc_id, convencional)
  g_bipartite_layout_base <- graph_from_data_frame(edge_list_layout_base, directed = FALSE)
  V(g_bipartite_layout_base)$type <- V(g_bipartite_layout_base)$name %in% df_layout_base$convencional
  proj_conv_layout_base <- bipartite_projection(g_bipartite_layout_base, which = TRUE)
} else {
  # Si no hay datos en el período final, usamos el grafo completo sin lazos para Louvain
  # O simplemente un grafo vacío para Louvain, pero es mejor que Louvain vea algo.
  # Para asegurar que Louvain pueda ejecutarse, si no hay actividad en la última semana,
  # usaremos la proyección completa para la detección de comunidades, aunque no es lo ideal.
  warning("No hay iniciativas en el período 28-Ene a 01-Feb. La detección de comunidades se realizará sobre la red completa.")
  proj_conv_layout_base <- proj_conv_full
}


# C. Detectar comunidades (Louvain) en la red de este período
communities_final <- cluster_louvain(proj_conv_layout_base)
cat(paste("Comunidades detectadas en el período final:", length(communities_final), "\n"))

# D. Re-ponderar los lazos del grafo COMPLETO para separar las comunidades
# Se crea una copia del grafo completo (que incluye todos los nodos) para no modificar el original
g_for_layout <- proj_conv_full
# Asignar el atributo de comunidad a todos los nodos del grafo completo.
# Si un nodo no estaba en el 'proj_conv_layout_base', su comunidad será NA.
V(g_for_layout)$community <- as.numeric(NA) # Inicializar todos como NA
# Asignar las comunidades a los nodos que sí fueron detectados.
V(g_for_layout)$community[match(names(communities_final$membership), V(g_for_layout)$name)] <- communities_final$membership

# Extraer el dataframe de aristas del grafo completo
edges_df_for_layout <- igraph::as_data_frame(g_for_layout, what = "edges")

# Pesos para lazos
edge_weights_for_layout <- edges_df_for_layout %>%
  mutate(
    # Acceder a V(g_for_layout)$community usando los nombres de los vértices en 'from' y 'to'
    from_comm = V(g_for_layout)$community[match(from, V(g_for_layout)$name)],
    to_comm = V(g_for_layout)$community[match(to, V(g_for_layout)$name)],
    # Si las comunidades son iguales y no son NA -> peso alto, de lo contrario -> peso bajo
    new_weight = if_else(!is.na(from_comm) & !is.na(to_comm) & from_comm == to_comm, 10, 0.1)
  ) %>%
  pull(new_weight)

# E. Calcular el layout final usando el grafo completo re-ponderado
cat("Calculando layout con separación de comunidades (puede tardar)... \n")
master_layout <- layout_with_fr(g_for_layout, weights = edge_weights_for_layout)
rownames(master_layout) <- V(g_for_layout)$name
cat("Layout maestro calculado.\n")

# --- Visualización de las Comunidades del Período Final ---

# Extraer los nombres de los nodos que están activos en este período específico
nodes_in_final_period <- V(proj_conv_layout_base)$name

# Filtrar el layout maestro para usar solo las coordenadas de los nodos activos
layout_for_final_plot <- master_layout[nodes_in_final_period, ]

plot(proj_conv_layout_base,
     layout = layout_for_final_plot,
     main = "Comunidades en el Período Final (28-Ene a 01-Feb)",
     # Colorear cada nodo según la comunidad a la que pertenece
     vertex.color = membership(communities_final),
     # Dibujar polígonos alrededor de cada comunidad
     mark.groups = communities_final,
     vertex.size = 5,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     edge.color = "gray50",
     # Usar la misma escala logarítmica para el grosor de los lazos
     edge.width = log(E(proj_conv_layout_base)$weight + 1) * 1.5
)


#############################################################################
# PARTE 2: GENERACIÓN DEL GIF CON LAYOUT Y LAZOS CORREGIDOS
#############################################################################
cat("\n--- PARTE 2: Generando GIF con lazos por bloque ---\n")

gif_dir <- "gif_frames"
if (dir.exists(gif_dir)) unlink(gif_dir, recursive = TRUE)
dir.create(gif_dir)

# Se crea el "grafo lienzo" con todos los nodos para asegurar consistencia
# Este grafo ya tiene el conjunto completo de todos los convencionales.
g_canvas <- delete_edges(proj_conv_full, E(proj_conv_full))

for (i in 1:length(week_blocks)) {
  block <- week_blocks[[i]]
  
  # Filtrar datos para el bloque específico (NO acumulativo)
  df_block_specific <- all_initiatives_df %>%
    filter(fecha >= block$start_date & fecha <= block$end_date)
  
  g_frame <- g_canvas # Inicia con todos los nodos pero sin lazos
  
  # Solo añadir lazos si hay actividad en este bloque
  if(nrow(df_block_specific) > 0) {
    edge_list_block <- df_block_specific %>% select(doc_id, convencional)
    g_bipartite_block <- graph_from_data_frame(edge_list_block, directed = FALSE)
    V(g_bipartite_block)$type <- V(g_bipartite_block)$name %in% df_block_specific$convencional
    proj_conv_block <- bipartite_projection(g_bipartite_block, which = TRUE)
    
    # Extraer los lazos y pesos de la proyección específica del bloque
    edges_for_frame <- igraph::as_data_frame(proj_conv_block, what = "edges")
    
    if (nrow(edges_for_frame) > 0) {
      edge_vector <- as.vector(t(as.matrix(edges_for_frame[, c("from", "to")])))
      # Add edges to the g_frame (which has all 154 nodes)
      g_frame <- add_edges(g_frame, edges = edge_vector, weight = edges_for_frame$weight)
    }
  }
  
  # Atributos de visualización para este frame
  node_degrees <- degree(g_frame) # Calcula el grado en la red de ESTE bloque
  
  # Nodos activos (con lazos en este bloque) serán de un color y tamaño, inactivos (sin lazos en este bloque) de otro
  V(g_frame)$color <- ifelse(node_degrees > 0, "lightblue", "grey90") # Color más claro para inactivos
  V(g_frame)$size <- ifelse(node_degrees > 0, 5, 2) # Tamaño más pequeño para inactivos
  # Solo mostrar etiquetas para nodos activos
  V(g_frame)$label <- ifelse(node_degrees > 0, V(g_frame)$name, NA)
  
  # Configurar el archivo PNG
  png_path <- file.path(gif_dir, sprintf("frame_%02d.png", i))
  png(png_path, width = 1200, height = 1100, res = 100) # Mantener proporciones cuadradas
  
  plot(g_frame,
       layout = master_layout, # Usar siempre el mismo layout maestro
       main = paste("Red de Patrocinantes -", block$label),
       vertex.label.color = "black",
       vertex.label.cex = 0.55,
       vertex.frame.color = NA, # Sin borde alrededor de los nodos
       edge.color = "gray60",
       edge.width = log(E(g_frame)$weight + 1) * 1.5
  )
  
  dev.off()
  cat(paste("Frame", i, "de", length(week_blocks), "generado:", block$label, "\n"))
}

# --- Compilación Final del GIF ---
frames_paths <- list.files(gif_dir, pattern = "frame_.*\\.png$", full.names = TRUE)
frames <- image_read(frames_paths)

animation <- image_animate(frames, fps = 1/1.2) # 1.2 segundos por frame

output_gif_path <- "red_patrocinantes_dinamica.gif"
image_write(animation, output_gif_path)

unlink(gif_dir, recursive = TRUE)

#############################################################################
# PARTE 3: RESPUESTAS A PREGUNTAS ANALÍTICAS (corrected_4)
#############################################################################

cat("\n--- PARTE 3: Cargando versión corrected_4 y respondiendo preguntas ---\n")

# Helper: parsear fechas robustamente con meses en español, ISO y variantes
# Helper: parser de fechas vectorizado (ES/ISO)
parse_fecha_es <- function(x) {
  x <- as.character(x)
  n <- length(x)
  if (n == 0) return(as.Date(character()))
  x <- trimws(tolower(x))
  x <- gsub("\\s+", " ", x)
  
  out <- as.Date(rep(NA_character_, n))
  
  # 1) ISO: YYYY[-/]MM[-/]DD
  iso_idx <- grepl("^\\d{4}[-/]\\d{2}[-/]\\d{2}$", x)
  if (any(iso_idx)) {
    out[iso_idx] <- suppressWarnings(lubridate::ymd(gsub("/", "-", x[iso_idx]), quiet = TRUE))
  }
  
  # 2) No-ISO: normalizar meses en español y separadores
  non_idx <- which(!iso_idx)
  if (length(non_idx) > 0) {
    meses <- c(
      "enero"="01","febrero"="02","marzo"="03","abril"="04","mayo"="05","junio"="06",
      "julio"="07","agosto"="08","septiembre"="09","setiembre"="09","octubre"="10",
      "noviembre"="11","diciembre"="12"
    )
    x2 <- x[non_idx]
    for (m in names(meses)) {
      x2 <- gsub(paste0("\\b", m, "\\b"), meses[[m]], x2, perl = TRUE)
    }
    x2 <- gsub("\\s*de\\s*", "/", x2, perl = TRUE)
    x2 <- gsub("-", "/", x2, fixed = TRUE)
    
    # 2.a dmy primero
    d1 <- suppressWarnings(lubridate::dmy(x2, quiet = TRUE))
    fill1 <- which(!is.na(d1))
    if (length(fill1) > 0) out[non_idx[fill1]] <- d1[fill1]
    
    # 2.b ymd por si quedó Y/M/D
    rem2 <- which(is.na(out[non_idx]))
    if (length(rem2) > 0) {
      d2 <- suppressWarnings(lubridate::ymd(gsub("/", "-", x2[rem2]), quiet = TRUE))
      fill2 <- which(!is.na(d2))
      if (length(fill2) > 0) out[non_idx[rem2[fill2]]] <- d2[fill2]
    }
  }
  
  # 3) Fallback: parse_date_time en lo que quede
  rem3 <- which(is.na(out))
  if (length(rem3) > 0) {
    d3 <- suppressWarnings(lubridate::parse_date_time(x[rem3], orders = c("dmy","ymd","dmY","Ymd"), quiet = TRUE))
    d3 <- as.Date(d3)
    fill3 <- which(!is.na(d3))
    if (length(fill3) > 0) out[rem3[fill3]] <- d3[fill3]
  }
  
  out
}

# --- 3.A Carga de JSON corrected_4 ---
data_folder_v4 <- "patrocinantes_identificacion"
json_files_v4 <- list.files(
  path = data_folder_v4,
  pattern = "api_extracted_\\d+_\\d+_corrected_4\\.json$",
  full.names = TRUE
)

if (length(json_files_v4) == 0) {
  warning("No se encontraron archivos corrected_4 en la carpeta 'patrocinantes_identificacion'.")
}

process_json_file_v4 <- function(file_path) {
  raw_data <- jsonlite::fromJSON(file_path)
  purrr::imap_dfr(raw_data, ~{
    # doc_id a partir de la clave del JSON (prefijo numérico)
    doc_id_match <- regexpr("^[0-9]+", .y)
    if (doc_id_match == -1) return(NULL)
    doc_id <- regmatches(.y, doc_id_match)

    # Helpers para asegurar longitud 1 y tipos correctos
    safe_chr1 <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      as.character(x[[1]])
    }
    safe_int1 <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_integer_)
      suppressWarnings(as.integer(x[[1]]))
    }

    # Campos explícitos en corrected_4
    firmantes <- .x$firmantes_matched
    # Aplanar por si viene como lista de vectores
    if (is.list(firmantes)) firmantes <- unlist(firmantes, use.names = FALSE)
    firmantes <- as.character(firmantes)

    if (is.null(firmantes) || length(firmantes) == 0) return(NULL)

    n <- length(firmantes)

    tibble::tibble(
      doc_id       = rep_len(doc_id, n),
      autor_matched= rep_len(safe_chr1(.x$autor_matched), n),
      comision     = rep_len(safe_chr1(.x$comision), n),
      comision_n   = rep_len(safe_int1(.x$comision_n), n),
      fecha_str    = rep_len(safe_chr1(.x$fecha), n),
      convencional = firmantes
    )
  })
}

all_initiatives_v4 <- dplyr::bind_rows(purrr::map(json_files_v4, process_json_file_v4))

if (nrow(all_initiatives_v4) == 0) {
  warning("No se pudo construir 'all_initiatives_v4'. Verifique el formato de los JSON corrected_4.")
} else {
  cat(paste("Total de patrocinios (v4):", nrow(all_initiatives_v4), "\n"))
}

# --- 3.B Limpieza de fechas ---
all_initiatives_v4 <- all_initiatives_v4 %>%
  dplyr::mutate(fecha = parse_fecha_es(fecha_str)) %>%
  dplyr::filter(!is.na(fecha)) %>%
  dplyr::filter(lubridate::year(fecha) >= 2020, lubridate::year(fecha) <= 2023)

if (nrow(all_initiatives_v4) == 0) {
  warning("No hay fechas válidas en v4 tras el parseo.")
}

# --- 3.B.1 Diagnóstico y clamp de rango esperado ---
rango_min <- lubridate::ymd("2021-11-09")
rango_max <- lubridate::ymd("2022-02-03")

# Guardar mapeo (fecha_str -> fecha parsed) para inspección
diagnostico_mapeo <- all_initiatives_v4 %>%
  dplyr::transmute(doc_id, convencional, fecha_str, fecha_parsed = fecha) %>%
  dplyr::arrange(fecha_parsed, fecha_str)

# Detectar fuera de rango
fuera_rango <- all_initiatives_v4 %>%
  dplyr::filter(fecha < rango_min | fecha > rango_max) %>%
  dplyr::arrange(fecha)
if (nrow(fuera_rango) > 0) {
  readr::write_csv(fuera_rango, "diagnostico_fechas_fuera_rango.csv")
  cat(paste0("  -> Advertencia: ", nrow(fuera_rango), " filas fuera del rango esperado. ",
             "Se guardó 'diagnostico_fechas_fuera_rango.csv'.\n"))
}

# Clamp: usar solo el rango válido para los cálculos posteriores
all_initiatives_v4 <- all_initiatives_v4 %>%
  dplyr::filter(fecha >= rango_min, fecha <= rango_max)

# Metadatos por iniciativa (doc_id), manteniendo comision_n y etiqueta
initiatives_meta_v4 <- all_initiatives_v4 %>%
  dplyr::distinct(doc_id, comision_n, comision, fecha_iniciativa = fecha)

#############################################################################
# PREGUNTA 1:
# ¿Hay convencionales que patrocinen en más de 1 comisión?
#############################################################################
cat("\n[Q1] Convencionales con patrocinios en múltiples comisiones...\n")

q1_df <- all_initiatives_v4 %>%
  dplyr::mutate(
    comision_n_clean = suppressWarnings(as.integer(comision_n)),
    comision_norm = dplyr::na_if(trimws(tolower(comision)), "")
  ) %>%
  dplyr::group_by(convencional) %>%
  dplyr::summarise(
    n_comisiones_distintas = dplyr::n_distinct(comision_n_clean, na.rm = TRUE),
    comisiones_n = paste(sort(unique(comision_n_clean[!is.na(comision_n_clean)])), collapse = " | "),
    comisiones_lbl = paste(sort(unique(comision_norm[!is.na(comision_norm)])), collapse = " | "),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(n_comisiones_distintas), convencional)

readr::write_csv(q1_df, "q1_comisiones_por_convencional.csv")

cat(paste0("  -> Archivo: q1_comisiones_por_convencional.csv (", nrow(q1_df), " filas)\n"))
#############################################################################
# PREGUNTA 2 y 3:
# Para cada convencional, ¿cuántos días pasaron desde el inicio del período
# observado hasta que firmaron su primer patrocinio?
# ¿Cuánto tiempo pasó hasta que TODOS los integrantes patrocinaron al menos 1 iniciativa?
#############################################################################
cat("\n[Q2] Días hasta el primer patrocinio por convencional...\n")

if (nrow(all_initiatives_v4) > 0) {
  periodo_inicio <- max(lubridate::ymd("2021-11-09"), min(all_initiatives_v4$fecha, na.rm = TRUE))

  q2_df <- all_initiatives_v4 %>%
    dplyr::filter(fecha >= rango_min, fecha <= rango_max) %>%
    dplyr::group_by(convencional) %>%
    dplyr::summarise(
      fecha_primer_patrocinio = min(fecha, na.rm = TRUE),
      dias_hasta_primer_patrocinio = as.integer(fecha_primer_patrocinio - periodo_inicio),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dias_hasta_primer_patrocinio, convencional)

  readr::write_csv(q2_df, "q2_dias_al_primer_patrocinio_por_convencional.csv")
  cat(paste0("  -> Archivo: q2_dias_al_primer_patrocinio_por_convencional.csv (", nrow(q2_df), " filas)\n"))
} else {
  warning("No hay datos en v4 para calcular Q2.")
}