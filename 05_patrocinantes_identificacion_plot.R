# Cargar las librerías
library(jsonlite)
library(dplyr)
library(purrr) # Para trabajar con listas de forma eficiente (map, map_dfr)
library(tidyr) # Para expandir listas en columnas (unnest, pivot_longer)
library(ggplot2) # Para crear los plots
library(vctrs) # Librería subyacente para bind_rows, útil tenerla cargada
library(readr) # Para parse_number
library(stringr) # Para formatear texto
library(grid) # For unit() y textGrob()
library(patchwork) # Para combinar el plot con el texto de resumen
library(cowplot) # Para extraer la leyenda (get_legend)

# --- Configuración Global ---
# Asegúrate de que estos directorios existan o créalos
BASE_JSON_DIR <- "patrocinantes_identificacion/"
BASE_PLOT_DIR <- "scripts - plots/"
if (!dir.exists(BASE_PLOT_DIR)) dir.create(BASE_PLOT_DIR, recursive = TRUE)


# Límite superior del eje Y para el conteo en Plot 1
Y_AXIS_LIMIT_PLOT1 = 16
Y_AXIS_AUTHOR_SYMBOLS_PLOT2 = 18 # Posición Y para símbolos de autor
Y_AXIS_ERROR_TRIANGLE_PLOT2 = 19 # Posición Y para triángulo de error
Y_AXIS_MAX_PLOT2 = 20            # Límite superior eje Y para Plot 2

# --- Definir los bloques de archivos a procesar ---
file_stems <- c(
  #"1000_1035"
  "900_999",
  "800_899",
  "700_799",
  "600_699",
  "500_599",
  "400_499",
  "300_399",
  "200_299",
  "100_199",
  "1_99"
)

# --- Inicio del Ciclo For ---
cat("\n--- INICIO DEL PROCESAMIENTO POR BLOQUES ---\n")

for (stem in file_stems) {
  
  cat(paste0("\n--- Procesando Bloque: ", stem, " ---\n"))
  
  # Construir rutas de archivos dinámicamente
  json_file_path <- file.path(BASE_JSON_DIR, paste0("api_extracted_", stem, ".json"))
  plot1_output_path <- file.path(BASE_PLOT_DIR, paste0("iniciativas_plot1_", stem, ".png"))
  plot2_output_path <- file.path(BASE_PLOT_DIR, paste0("iniciativas_plot2_", stem, ".png"))
  
  # --- Cargar y Procesar el Archivo JSON ---
  cat(paste0("Intentando cargar y analizar el archivo: '", json_file_path, "'\n"))
  if (!file.exists(json_file_path)) {
    warning(paste0("Advertencia: El archivo '", json_file_path, "' no fue encontrado. Saltando este bloque."))
    next
  }
  
  results_list <- tryCatch({
    read_json(json_file_path, simplifyVector = FALSE)
  }, error = function(e) {
    warning(paste0("Error al cargar o parsear el archivo JSON '", json_file_path, "': ", e$message, ". Saltando este bloque."))
    return(NULL)
  })
  
  if (is.null(results_list)) next
  
  cat(paste0("Archivo '", json_file_path, "' cargado exitosamente.\n"))
  
  if (!is.list(results_list) || length(results_list) == 0) {
    warning(paste0("El archivo JSON '", json_file_path, "' no contiene datos o no tiene la estructura esperada. Saltando este bloque."))
    next
  }
  
  # Convertir la lista de resultados en un data frame robusto
  results_df <- map_dfr(results_list, .id = "filename", function(entry) {
    propuesta_norma <- entry$propuesta_norma %||% NA_character_
    error <- entry$error %||% NA_character_
    autor <- entry$autor %||% NA_character_
    autor_matched <- entry$autor_matched %||% NA_character_
    error_limpieza <- entry$error_limpieza %||% NA_character_
    firmantes <- entry$firmantes %||% list()
    firmantes_matched <- entry$firmantes_matched %||% list()
    firmantes_not_matched <- entry$firmantes_not_matched %||% list()
    n_firmantes <- as.numeric(entry$n_firmantes %||% 0)
    n_firmantes_matched <- as.numeric(entry$n_firmantes_matched %||% 0) # Calculado o desde JSON? Asumamos desde JSON
    n_firmantes_not_matched <- as.numeric(entry$n_firmantes_not_matched %||% 0)
    
    if (!is.list(firmantes)) firmantes <- list()
    if (!is.list(firmantes_matched)) firmantes_matched <- list()
    if (!is.list(firmantes_not_matched)) firmantes_not_matched <- list()
    
    if (is.na(n_firmantes)) n_firmantes <- 0
    if (is.na(n_firmantes_matched)) n_firmantes_matched <- 0
    if (is.na(n_firmantes_not_matched)) n_firmantes_not_matched <- 0
    
    # Opcional: Recalcular n_firmantes_matched si es necesario
    # n_firmantes_matched <- n_firmantes - n_firmantes_not_matched
    
    list(
      propuesta_norma = as.character(propuesta_norma),
      error = as.character(error),
      autor = as.character(autor),
      autor_matched = as.character(autor_matched),
      firmantes = list(firmantes),
      firmantes_matched = list(firmantes_matched),
      firmantes_not_matched = list(firmantes_not_matched),
      n_firmantes = n_firmantes,
      n_firmantes_matched = n_firmantes_matched, # Usamos este valor
      n_firmantes_not_matched = n_firmantes_not_matched,
      error_limpieza = as.character(error_limpieza)
    )
  })
  
  total_files_in_json_raw <- length(results_list)
  cat(paste0("\nTotal de archivos procesados registrados en este JSON (Bloque ", stem, "): ", total_files_in_json_raw, "\n"))
  
  # --- Extraer el número inicial del nombre del archivo ---
  results_df <- results_df %>%
    mutate(file_number_raw = str_extract(filename, "^[0-9]+")) %>%
    mutate(file_number = readr::parse_number(file_number_raw)) %>%
    filter(!is.na(file_number) & file_number == floor(file_number)) %>%
    arrange(file_number)
  
  total_files_valid_number <- nrow(results_df)
  if (total_files_valid_number == 0) {
    warning(paste0("No se pudo extraer un número inicial válido de ningún nombre de archivo para graficar en el bloque ", stem, ". Saltando este bloque."))
    next
  }
  cat(paste0("Archivos con número inicial válido para graficar (Bloque ", stem, "): ", total_files_valid_number, "\n"))
  
  # --- Estadísticas de Resumen ---
  total_autor_any = sum(!is.na(results_df$autor) & results_df$autor != "")
  total_autor_matched = sum(!is.na(results_df$autor_matched) & results_df$autor_matched != "")
  total_firmantes_any = sum(results_df$n_firmantes, na.rm = TRUE)
  # total_firmantes_matched = sum(results_df$n_firmantes_matched, na.rm = TRUE) # Si existe la columna y es confiable
  # O calcularlo:
  total_firmantes_matched_calc = total_firmantes_any - sum(results_df$n_firmantes_not_matched, na.rm = TRUE)
  
  cat("\n--- Estadísticas de Resumen (Bloque ", stem, ") --- \n")
  cat(paste0("- Con autor: ", total_autor_any, "/", total_files_valid_number, " -> ", round(total_autor_any/total_files_valid_number,2), "\n"))
  cat(paste0("- Con autor_matched: ", total_autor_matched, "/", total_files_valid_number, " -> ", round(total_autor_matched/total_files_valid_number, 2), "\n"))
  cat(paste0("- Total patrocinadores identificados (n_firmantes): ", total_firmantes_any, "\n"))
  cat(paste0("- Total patrocinadores matched (n_firmantes_matched): ", total_firmantes_matched, "\n")) # Si usas la columna directa
  cat(paste0("- Total patrocinadores matched (calculado): ", total_firmantes_matched_calc, "\n")) # Si lo calculas
  cat(paste0("- Total patrocinadores no matched (n_firmantes_not_matched): ", sum(results_df$n_firmantes_not_matched, na.rm = TRUE), "\n"))
  
  
  # Crear el texto para el 'pseudo-leyenda' de resumen Y error
  summary_legend_text <- paste0(
    "Resumen Global (Bloque ", stem, "):\n", "\n",
    "-Con autor: ", total_autor_any, "/", total_files_valid_number, " -> ", round(total_autor_any/total_files_valid_number,2), "\n",
    "-Con autor_matched: ", total_autor_matched, "/", total_files_valid_number, " -> ", round(total_autor_matched/total_files_valid_number, 2), "\n",
    #"-Total patrocinadores: ", total_firmantes_any, "\n",
    "-Patroc. matched: ", total_firmantes_matched_calc, "/", total_firmantes_any, " -> ", round(total_firmantes_matched_calc/total_firmantes_any, 2), "\n",
    "-Patroc. not matched: ",  sum(results_df$n_firmantes_not_matched, na.rm = TRUE), "/", total_firmantes_any, " -> ", round( sum(results_df$n_firmantes_not_matched, na.rm = TRUE)/total_firmantes_any, 2), "\n",
    "\n", "\n",
    "Indicador Error (△):\n", "\n",
    "-Relleno Negro: Con Error\n",
    "-Relleno Blanco: Sin Error"
  )
  
  # --- Plot 1: (Sin cambios) ---
  cat("\n--- Generando Plot 1 (Bloque ", stem, "): Firmantes No Coincidentes por Número de Archivo (Vertical) ---\n")
  plot1_data <- results_df
  plot1_data$file_number_factor <- factor(plot1_data$file_number, levels = sort(unique(plot1_data$file_number)))
  plot1_title <- paste0("Firmantes No Coincidentes por Archivo (Bloque ", stem, ", Máx ", Y_AXIS_LIMIT_PLOT1, ")")
  
  plot1 <- ggplot(plot1_data, aes(x = file_number_factor, y = n_firmantes_not_matched)) +
    geom_col(fill = "skyblue", width = 0.7) + # Ajustar width si se desea
    geom_text(aes(label = ifelse(n_firmantes_not_matched > 0, n_firmantes_not_matched, "")), vjust = -0.5, size = 2.5) +
    scale_y_continuous(limits = c(0, Y_AXIS_LIMIT_PLOT1), expand = expansion(mult = c(0, 0.05)),
                       breaks = seq(0, Y_AXIS_LIMIT_PLOT1, by = 4)) +
    labs(
      title = plot1_title,
      x = "Número de Archivo", y = "Nº Firmantes No Coincidentes"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size=14),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
      axis.title.x = element_text(size=12),
      axis.title.y = element_text(size=12)
    )
  ggsave(plot1_output_path, plot = plot1, width = 15, height = 8.5, dpi = 500)
  cat(paste0("Plot 1 guardado en: '", plot1_output_path, "'\n"))
  
  # --- Plot 2: Comparativa Firmantes (Superpuestos) + Indicadores ---
  cat("\n--- Generando Plot 2 (Bloque ", stem, "): Comparativa Firmantes Superpuestos + Indicadores ---\n")
  
  # Definir niveles del factor ANTES para asegurar orden en todo
  file_number_levels <- sort(unique(results_df$file_number))
  results_df$file_number_factor <- factor(results_df$file_number, levels = file_number_levels)
  
  # Preparar datos para puntos indicadores (autor Y error)
  plot2_data_points <- results_df %>%
    select(file_number, file_number_factor, autor, autor_matched, error) %>%
    distinct(file_number, .keep_all = TRUE) %>%
    mutate(
      author_symbol = factor(case_when(
        !is.na(autor_matched) & autor_matched != "" ~ "Autor_Matched",
        !is.na(autor) & autor != "" ~ "Autor_Not_Matched",
        TRUE ~ "No_Autor"
      ), levels = c("Autor_Matched", "Autor_Not_Matched", "No_Autor"))
    ) %>%
    mutate(has_error = !is.na(error) & error != "")
  
  # --- Definiciones para leyenda ---
  # Autor (Shapes)
  author_legend_breaks <- c("Autor_Matched", "Autor_Not_Matched", "No_Autor")
  author_legend_labels <- c("Autor Matcheado", "Autor No Matcheado", paste0("\u2205 ", "Sin Autor"))
  author_shapes <- c("Autor_Matched" = 19, "Autor_Not_Matched" = 1, "No_Autor" = NA) # 19=sólido, 1=hueco
  author_override_shapes <- c(19, 1, NA)
  # Para la leyenda de shape, necesitamos especificar cómo deben verse:
  author_override_legend_aes <- list(
    shape = author_override_shapes,
    color = c("black", "black", NA), # Borde negro para ambos visibles
    fill = c("black", "white", NA)  # Relleno negro para 19, blanco para 1
  )
  
  
  # Firmantes (Fill para las barras)
  firmantes_legend_breaks <- c("n_firmantes", "n_firmantes_not_matched")
  firmantes_legend_labels <- c("Patrocinadores", "Patrocinadores not matched")
  firmantes_colors <- c("n_firmantes" = "green", "n_firmantes_not_matched" = "red")
  
  # --- Título dinámico para Plot 2 ---
  plot2_title <- paste0("Resumen procesamiento Indicaciones ", stem, ".")
  
  # --- Crear el plot base ---
  create_base_plot2 <- function(include_legend = TRUE) {
    p <- ggplot(results_df, aes(x = file_number_factor)) +
      
      # Capa 1: Barras verdes (Total Firmantes) - Detrás
      # Mapea fill a la cadena "n_firmantes"
      geom_col(aes(y = n_firmantes, fill = "n_firmantes"), width = 0.7) +
      
      # Capa 2: Barras rojas (No Coincidentes) - Delante
      # Mapea fill a la cadena "n_firmantes_not_matched"
      geom_col(aes(y = n_firmantes_not_matched, fill = "n_firmantes_not_matched"), width = 0.7) +
      
      # Capa 3: Puntos indicadores de Autor
      # Mapea shape a author_symbol. Color es fijo (negro).
      geom_point(data = filter(plot2_data_points, author_symbol != "No_Autor"),
                 aes(y = Y_AXIS_AUTHOR_SYMBOLS_PLOT2, shape = author_symbol),
                 size = 3, stroke = 0.5, color = "black") + # Color fijo
      
      # Capa 4: Puntos indicadores de Error (Triángulo)
      # Shape fijo (24). Fill se establece directamente, no se mapea en aes().
      geom_point(data = plot2_data_points,
                 aes(y = Y_AXIS_ERROR_TRIANGLE_PLOT2), # Sin mapeo de fill/shape aquí
                 shape = 24, # Triángulo hacia arriba
                 color = "black", # Borde negro
                 fill = ifelse(plot2_data_points$has_error, "black", "white"), # Relleno directo
                 size = 3, stroke = 0.5) +
      
      # Escala para Fill (Aplicada a las barras por geom_col aes())
      scale_fill_manual(
        name = "Firmantes",
        values = firmantes_colors, # Usa el vector definido: c(n_firmantes="darkgreen", n_firmantes_not_matched="red")
        breaks = firmantes_legend_breaks,
        labels = firmantes_legend_labels,
        drop = FALSE
      ) +
      
      # Escala para Shape (Aplicada a los puntos de autor por geom_point aes())
      scale_shape_manual(
        name = "Indicador Autor",
        values = author_shapes,
        breaks = author_legend_breaks,
        labels = author_legend_labels,
        drop = FALSE
      ) +
      
      # Guías (Leyendas)
      guides(
        # Leyenda para Fill (barras)
        fill = guide_legend(title = "Firmantes", order = 1, override.aes = list(shape = NA)), # Oculta shapes aquí
        # Leyenda para Shape (autor)
        shape = guide_legend(title = "Indicador Autor", order = 2,
                             override.aes = author_override_legend_aes # Usa la lista definida arriba
        )
      ) +
      
      # Resto de escalas y tema
      scale_y_continuous(limits = c(0, Y_AXIS_MAX_PLOT2), expand = expansion(mult = c(0, 0.05)), breaks = seq(0, Y_AXIS_MAX_PLOT2, by = 4)) +
      scale_x_discrete(expand = expansion(add = 0.5)) +
      labs(title = plot2_title, x = "Número de Archivo", y = "Número de Firmantes", caption = NULL) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size=14),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.caption = element_blank(),
        legend.position = if (include_legend) "right" else "none",
        legend.box = "vertical"
      )
    return(p)
  }
  
  # Crear plots para leyenda y principal
  plot2_for_legend <- create_base_plot2(include_legend = TRUE)
  plot2_main <- create_base_plot2(include_legend = FALSE)
  
  # Extraer leyenda
  extracted_legend <- cowplot::get_legend(plot2_for_legend)
  if (is.null(extracted_legend)) {
    warning("No se pudo extraer la leyenda para el bloque ", stem)
    # Puedes decidir continuar sin leyenda o detenerte
    extracted_legend <- plot_spacer() # Usar un espacio en blanco si falla
  }
  
  
  # Crear grob de resumen
  summary_grob <- grid::textGrob(
    summary_legend_text,
    x = 0.1, y = 0.95, hjust = 0, vjust = 1,
    gp = grid::gpar(fontsize = 9, lineheight = 1.2)
  )
  
  # Combinar leyenda y resumen
  right_panel <- patchwork::wrap_plots(
    extracted_legend,
    summary_grob,
    ncol = 1,
    heights = c(0.55, 0.45) # Ajusta según sea necesario
  )
  
  # Combinar plot principal y panel derecho
  final_plot2 <- plot2_main + right_panel +
    plot_layout(widths = c(6, 1)) # Ajusta proporción
  
  # Guardar Plot 2
  ggsave(plot2_output_path, plot = final_plot2, width = 17, height = 8.5, dpi = 500)
  cat(paste0("Plot 2 guardado en: '", plot2_output_path, "'\n"))
  
  cat(paste0("--- Bloque ", stem, " procesado exitosamente. ---\n"))
  
} # Fin del ciclo for

cat("\n--- FIN DEL PROCESAMIENTO POR BLOQUES ---\n")