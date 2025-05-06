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


# 3. Create the bipartite graph (igraph)

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


# 5. Network Projections

proj_conv <- bipartite_projection(g_bipartite, which = TRUE)
E(proj_conv)$weight
summary(proj_conv)

proj_docs <- bipartite_projection(g_bipartite, which = FALSE)
E(proj_docs)$weight
summary(proj_docs)


# Patrocinantes Network
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

# Iniciativas Network
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
