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

# Abrir un nuevo dispositivo gráfico (opcional, pero bueno si trabajas en RStudio)
# dev.new() 

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

cat("Plot estático generado.\n")

# --- FIN DEL CÓDIGO A AGREGAR ---

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
cat("Compilando frames en el archivo GIF...\n")
frames_paths <- list.files(gif_dir, pattern = "frame_.*\\.png$", full.names = TRUE)
frames <- image_read(frames_paths)

animation <- image_animate(frames, fps = 1/1.2) # 1.2 segundos por frame

output_gif_path <- "red_patrocinantes_dinamica.gif"
image_write(animation, output_gif_path)

cat(paste("\n¡Proceso completado! El GIF ha sido guardado como:", output_gif_path, "\n"))
unlink(gif_dir, recursive = TRUE)
