library(jsonlite)
library(igraph)
library(dplyr)
library(purrr)
library(lubridate)
library(magick)

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

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
  list(end_date = ymd("2021-11-19"), label = "Semana 1 (Hasta 19-Nov-21)"),
  list(end_date = ymd("2021-11-28"), label = "Semana 2 (Hasta 28-Nov-21)"),
  list(end_date = ymd("2021-12-05"), label = "Semana 3 (Hasta 05-Dic-21)"),
  list(end_date = ymd("2021-12-12"), label = "Semana 4 (Hasta 12-Dic-21)"),
  list(end_date = ymd("2021-12-19"), label = "Semana 5 (Hasta 19-Dic-21)"),
  list(end_date = ymd("2021-12-26"), label = "Semana 6 (Hasta 26-Dic-21)"),
  list(end_date = ymd("2022-01-02"), label = "Semana 7 (Hasta 02-Ene-22)"),
  list(end_date = ymd("2022-01-09"), label = "Semana 8 (Hasta 09-Ene-22)"),
  list(end_date = ymd("2022-01-16"), label = "Semana 9 (Hasta 16-Ene-22)"),
  list(end_date = ymd("2022-01-23"), label = "Semana 10 (Hasta 23-Ene-22)"),
  list(end_date = ymd("2022-02-01"), label = "Semana 11 (Hasta 01-Feb-22)")
)

# --- 4. Creación del Grafo Lienzo y Layout Maestro ---
edge_list_full <- all_initiatives_df %>% select(doc_id, convencional)
g_bipartite_full <- graph_from_data_frame(edge_list_full, directed = FALSE)
V(g_bipartite_full)$type <- V(g_bipartite_full)$name %in% all_initiatives_df$convencional
proj_conv_full <- bipartite_projection(g_bipartite_full, which = TRUE)

# **MEJORA 1**: Crear un "grafo lienzo" con todos los nodos pero sin lazos.
g_canvas <- delete_edges(proj_conv_full, E(proj_conv_full))

cat(paste("Grafo lienzo creado con", vcount(g_canvas), "nodos (convencionales).\n"))

# Calcular el layout maestro usando Kamada-Kawai
cat("Calculando layout maestro...\n")
master_layout <- layout_with_kk(proj_conv_full)
rownames(master_layout) <- V(proj_conv_full)$name
cat("Layout maestro calculado.\n")

# --- 5. Generación de Frames para el GIF ---
gif_dir <- "gif_frames"
if (dir.exists(gif_dir)) unlink(gif_dir, recursive = TRUE)
dir.create(gif_dir)
cat("Generando frames para el GIF...\n")

for (i in 1:length(week_blocks)) {
  block <- week_blocks[[i]]
  df_cumulative <- all_initiatives_df %>% filter(fecha <= block$end_date)
  
  # Inicializa el grafo del frame como una copia del lienzo (todos los nodos, sin lazos)
  g_frame <- g_canvas
  
  # Si hay datos en este período, calcula la proyección y añade los lazos
  if(nrow(df_cumulative) > 0) {
    edge_list_block <- df_cumulative %>% select(doc_id, convencional)
    g_bipartite_block <- graph_from_data_frame(edge_list_block, directed = FALSE)
    V(g_bipartite_block)$type <- V(g_bipartite_block)$name %in% df_cumulative$convencional
    proj_conv_block <- bipartite_projection(g_bipartite_block, which = TRUE)
    
    # Extraer lazos y pesos del grafo del bloque
    edges_for_frame <- as_data_frame(proj_conv_block, what = "edges")
    
    # Si existen lazos en este bloque, añadirlos al grafo del frame
    if (nrow(edges_for_frame) > 0) {
      # Crear el vector de lazos para add_edges()
      edge_vector <- as.vector(t(as.matrix(edges_for_frame[, c("from", "to")])))
      g_frame <- add_edges(g_frame, edges = edge_vector, weight = edges_for_frame$weight)
    }
  }
  
  # --- Atributos de visualización para este frame ---
  # Los nodos activos (con lazos) serán más grandes y de color; los inactivos, pequeños y grises
  node_degrees <- degree(g_frame)
  V(g_frame)$color <- ifelse(node_degrees > 0, "lightblue", "grey85")
  V(g_frame)$size <- ifelse(node_degrees > 0, 4.5, 2)
  V(g_frame)$label <- ifelse(node_degrees > 0, V(g_frame)$name, NA) # Ocultar etiquetas de nodos inactivos
  
  # Guardar el frame en un archivo PNG
  png_path <- file.path(gif_dir, sprintf("frame_%02d.png", i))
  png(png_path, width = 1200, height = 1100, res = 100)
  
  plot(g_frame,
       layout = master_layout, # Usar siempre el mismo layout maestro
       main = paste("Red de Patrocinantes -", block$label),
       vertex.label.color = "black",
       vertex.label.cex = 0.6,
       vertex.frame.color = NA,
       edge.color = "gray60",
       # **MEJORA 2**: Aplicar escala logarítmica al grosor de los lazos
       edge.width = log(E(g_frame)$weight + 1) * 1.5 
  )
  
  dev.off()
  cat(paste("Frame", i, "de", length(week_blocks), "generado:", block$label, "\n"))
}

# --- 6. Compilación del GIF ---
cat("Compilando frames en el archivo GIF...\n")
frames_paths <- list.files(gif_dir, pattern = "frame_.*\\.png$", full.names = TRUE)
frames <- image_read(frames_paths)

# Crear la animación (1.2 segundos por frame)
animation <- image_animate(frames, fps = 1/1.2)

output_gif_path <- "red_dinamica_patrocinantes_v2.gif"
image_write(animation, output_gif_path)

cat(paste("\n¡Proceso completado! El GIF ha sido guardado como:", output_gif_path, "\n"))
unlink(gif_dir, recursive = TRUE) # Limpiar carpeta temporal