library(jsonlite)
library(igraph)
library(dplyr)
library(purrr)

# 1. Import
json_data <- fromJSON("patrocinantes_identificacion/api_extracted_600_699_corrected_2.json")

# VAlidation
validation_passed <- TRUE
invalid_docs <- c()

for (doc_name in names(json_data)) {
  entry <- json_data[[doc_name]]
  
  is_not_matched_empty <- length(entry$firmantes_not_matched) == 0 # "firmantes_not_matched" = []
  is_n_not_matched_zero <- entry$n_firmantes_not_matched == 0 # "n_firmantes_not_matched" = 0
  
  if (!(is_not_matched_empty && is_n_not_matched_zero)) {
    validation_passed <- FALSE
    invalid_docs <- c(invalid_docs, doc_name)
    warning(paste("Validation failed for document:", doc_name,
                  "- firmantes_not_matched length:", length(entry$firmantes_not_matched),
                  "- n_firmantes_not_matched:", entry$n_firmantes_not_matched))
  }
}


# 2. Data Extraction

edge_list_df <- imap_dfr(json_data, ~{
  doc_id_match <- regexpr("^[0-9]+", .y)
  if (doc_id_match == -1) {
    warning(paste("Could not extract numeric ID from document key:", .y))
    return(NULL)
  }
  doc_id <- regmatches(.y, doc_id_match)
  firmantes <- .x$firmantes_matched
  
  if (length(firmantes) > 0) {
    tibble(
      doc_id = doc_id,
      convencional = firmantes
    )
  } else {
    tibble(doc_id = character(), convencional = character())
  }
})

# Store unique names BEFORE creating the graph to assign types later
unique_docs <- unique(edge_list_df$doc_id)
unique_convs <- unique(edge_list_df$convencional)

print(paste("Created edge list with", nrow(edge_list_df), "edges."))
print(paste("Unique document IDs found:", length(unique_docs)))
print(paste("Unique conventionals found:", length(unique_convs)))


# 3. Create the bipartite graph

g_bipartite <- graph_from_data_frame(edge_list_df, directed = FALSE)

# --- !!! Explicitly set the 'type' attribute !!! ---
# Assign 'type' based on whether the vertex name is in the list of conventionals.
# Conventionals = TRUE, Documents = FALSE
V(g_bipartite)$type <- V(g_bipartite)$name %in% unique_convs

# --- Assign labels based on the manually set type ---
V(g_bipartite)$type_label <- ifelse(V(g_bipartite)$type, "Convencional", "Documento")

sum(!V(g_bipartite)$type)
sum(V(g_bipartite)$type)


# 4. Visualize the bipartite graph
V(g_bipartite)$color <- ifelse(V(g_bipartite)$type, "lightblue", "salmon")
V(g_bipartite)$shape <- ifelse(V(g_bipartite)$type, "circle", "square")
V(g_bipartite)$label.cex <- 0.7
V(g_bipartite)$label.color <- "black"
V(g_bipartite)$size <- 6
E(g_bipartite)$color <- "lightgray"

# Use a bipartite layout
l_bi <- layout_as_bipartite(g_bipartite)

plot(g_bipartite,
     layout = l_bi,
     vertex.label.dist = 0.5,
     vertex.label.degree = -pi/2,
     main = "Red Bipartita: Documentos - Convencionales Firmantes"
     # vertex.label = NA # Uncomment to hide labels if too crowded
)

legend("topleft", legend=c("Documento", "Convencional"),
       pch=c(15, 16),
       col=c("salmon", "lightblue"), pt.cex=2, cex=0.8, bty="n")


# 5. Network Projections =======================================================

proj_conv <- bipartite_projection(g_bipartite, which = TRUE)
E(proj_conv)$weight
summary(proj_conv)

proj_docs <- bipartite_projection(g_bipartite, which = FALSE)
E(proj_docs)$weight
summary(proj_docs)


# Patrocinantes Network --------------------------------------------------------
V(proj_conv)$size <- 4
V(proj_conv)$label.cex <- 0.6
l_conv <- layout_with_fr(proj_conv)

plot(proj_conv,
     layout = l_conv,
     main = "Red de Patrocinantes",
     vertex.color = "lightblue", # Consistent color
     vertex.shape = "circle",    # Consistent shape
     vertex.label.color="black",
     edge.width = E(proj_conv)$weight / mean(E(proj_conv)$weight), # Normalize width a bit
     edge.color = "gray70"
     # vertex.label = NA # Uncomment to hide labels if too crowded
)

# filtramos por peso
umbral_de_peso <- 5
proj_conv_filtrado <- delete_edges(proj_conv, E(proj_conv)[weight <= umbral_de_peso])

# remover nodos aislados
proj_conv_filtrado <- delete_vertices(proj_conv_filtrado, which(degree(proj_conv_filtrado) == 0))

# Se recalcula el layout para el grafo filtrado, ya que su estructura ha cambiado
l_conv_filtrado <- layout_with_kk(proj_conv_filtrado)

# Plot del grafo filtrado
plot(proj_conv_filtrado,
     layout = l_conv_filtrado,
     main = paste("Red de Patrocinantes (Lazos con más de", umbral_de_peso, 
                  "iniciativas en común)"),
     vertex.color = "lightblue",
     vertex.shape = "circle",
     vertex.size = 4,
     vertex.label.cex = 0.6,
     vertex.label.color="black",
     # El grosor del lazo ahora se basa en el grafo filtrado
     edge.width = E(proj_conv_filtrado)$weight / mean(E(proj_conv_filtrado)$weight),
     edge.color = "gray70"
)

# Iniciativas Network ----------------------------------------------------------
V(proj_docs)$size <- 4
V(proj_docs)$label.cex <- 0.5
l_docs <- layout_with_fr(proj_docs)

plot(proj_docs,
     layout = l_docs,
     main = "Red de Iniciativas Constitucionales",
     vertex.color = "salmon", 
     vertex.shape = "square", 
     vertex.label.color="black",
     edge.width = E(proj_docs)$weight / mean(E(proj_docs)$weight), # Normalize width a bit
     edge.color = "gray70"
     # vertex.label = NA # Uncomment to hide labels if too crowded
)

#--- Líneas para el Arreglo de Subplots de Detección de Comunidades ---

# Se guarda el layout original para usarlo en todos los subplots.
l_docs <- layout_with_fr(proj_docs)

# Se configura el dispositivo gráfico para un arreglo de 2x2.
# mar=c(1, 1, 2, 1) reduce los márgenes para dar más espacio a los gráficos.
par(mfrow = c(2, 2), mar = c(1, 1, 2, 1))

# --- Subplot 1: Algoritmo Walktrap (el que ya tenías) ---
comunidades_walktrap <- cluster_walktrap(proj_docs)
plot(proj_docs, layout = l_docs, main = "Comunidades con Walktrap",
     vertex.size = 4, vertex.label = NA, # Se quitan las etiquetas para mayor claridad
     vertex.color = membership(comunidades_walktrap),
     mark.groups = comunidades_walktrap)

# --- Subplot 2: Algoritmo Louvain ---
comunidades_louvain <- cluster_louvain(proj_docs)
plot(proj_docs, layout = l_docs, main = "Comunidades con Louvain",
     vertex.size = 4, vertex.label = NA,
     vertex.color = membership(comunidades_louvain),
     mark.groups = comunidades_louvain)

# --- Subplot 3: Algoritmo Fast Greedy ---
comunidades_fastgreedy <- cluster_fast_greedy(proj_docs)
plot(proj_docs, layout = l_docs, main = "Comunidades con Fast Greedy",
     vertex.size = 4, vertex.label = NA,
     vertex.color = membership(comunidades_fastgreedy),
     mark.groups = comunidades_fastgreedy)

# --- Subplot 4: Algoritmo Infomap ---
comunidades_infomap <- cluster_infomap(proj_docs)
plot(proj_docs, layout = l_docs, main = "Comunidades con Infomap",
     vertex.size = 4, vertex.label = NA,
     vertex.color = membership(comunidades_infomap),
     mark.groups = comunidades_infomap)

# Se restaura el dispositivo gráfico a su configuración original (un solo plot).
par(mfrow = c(1, 1))
