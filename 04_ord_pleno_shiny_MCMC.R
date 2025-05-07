# --- 0. Load Libraries ---
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(patchwork)
library(forcats) # For reordering factors

# --- 1. Load and Prepare Data -------------------------------------------------
tryCatch({
  #orden_votantes_t_MCMC <- read_csv("03_orden_votantes_t_MCMC.csv", show_col_types = FALSE)
  orden_votantes_t_MCMC <- readRDS("03_orden_votantes_t_MCMC.rds")
}, error = function(e) {
  stop("Error loading 03_orden_votantes_t_MCMC. Make sure the file exists in the app directory. Original error: ", e$message)
})

# Pre-process the data
orden_votantes_t_MCMC_procesado <- orden_votantes_t_MCMC %>%
  mutate(
    # To double-check normalization in names
    Votante = str_squish(Votante), 
    Periodo = str_squish(Periodo1),
    comparacion = str_squish(comparacion),
    
    # Calculate plot variables
    posicion_inicial = pos_ideol_inicial, # Position in the (first) period of comparison
    #posicion_final = posicion_ideologica + diferencia_media, # WRONG !!
    posicion_final = pos_ideol_inicial + dif_media, # Position in the (second) period of comparison
    direccion_cambio = case_when(
      dif_media > 0 ~ "Derecha",
      dif_media < 0 ~ "Izquierda",
      TRUE ~ "Sin cambio"
    ),
    significativo = p_valor < 0.05,
    etiqueta_significancia = ifelse(significativo, "Significativo (p < 0.05)", "No Significativo (p >= 0.05)"),
    
    # Create factors for consistent plotting legends/colors
    direccion_cambio_factor = factor(direccion_cambio, levels = c("Izquierda", "Derecha", "Sin cambio")),
    etiqueta_significancia_factor = factor(etiqueta_significancia, levels = c("Significativo (p < 0.05)", "No Significativo (p >= 0.05)"))
  ) %>%
  # Extract the two periods from the 'comparacion' string
  tidyr::separate(comparacion, into = c("Periodo1", "Periodo2"), sep = " vs ", remove = FALSE)


# Get unique periods available, to show them later
available_periods <- unique(c(orden_votantes_t_MCMC_procesado$Periodo1, orden_votantes_t_MCMC_procesado$Periodo2))
available_periods <- sort(available_periods) # Sort them for the dropdown

if (length(available_periods) == 0) {
  stop("No periods found in the data. Check the 'comparacion' column format in your Data.")
}

# --- 2. Define UI -------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Cambios en Ordenamiento Político según bloques de sesiones en el Pleno — CC 2021-2022"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Bloques de sesiones del Pleno a Comparar"),
      
      br(), # spacing
      
      # --- Input for Comparison 1 (Top Plot) ---
      h5("Comparación 1 (Gráfico Superior):"),
      selectInput("period1_start",
                  "Periodo Inicial:",
                  choices = available_periods,
                  selected = available_periods[1]), # Default to the first
      selectInput("period1_end",
                  "Periodo Final:",
                  choices = available_periods,
                  selected = if(length(available_periods) > 1) available_periods[2] else available_periods[1]), # Default to the second
      
      br(), # spacing
      
      # --- Input for Comparison 2 (Bottom Plot) ---
      h5("Comparación 2 (Gráfico Inferior):"),
      selectInput("period2_start",
                  "Periodo Inicial:",
                  choices = available_periods,
                  # Sensible default: maybe the same as period1_start or the next one
                  selected = available_periods[1]), 
      selectInput("period2_end",
                  "Periodo Final:",
                  choices = available_periods,
                  # Sensible default: maybe the last or thrird available
                  selected = if(length(available_periods) > 2) available_periods[length(available_periods)] else available_periods[3]),
      
      hr(),
      p("Tanto el ordenamiento político como los bootstrap se calcularon mediante ", strong("IDEAL.")),
      p(strong("El orden del Eje X"), " se basa en la posición de los votantes en el 'Periodo Inicial' de la Comparación 1 (gráfico superior)."),
      p(strong("Las flechas"), " indican la dirección del cambio (Rojo: Izquierda, Azul: Derecha)."),
      p(strong("La forma del punto"), " indica significancia estadística en el test-t (Círculo: Significativo p<0.05, Triángulo: No Significativo).")
    ),
    
    mainPanel(
      br(), # spacing
      
      # Output for the combined plot
      plotOutput("combined_plot", height = "700px") # Adjust height as needed
    )
  )
)

# --- 3. Define Server Logic ---------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive expression to generate the plot data based on selections
  plot_data <- reactive({
    # Ensure all four inputs are selected before proceeding
    req(input$period1_start, input$period1_end, input$period2_start, input$period2_end)
    
    # Prevent user from selecting the same start and end period for a comparison
    validate(
      need(input$period1_start != input$period1_end, "Comparación 1: El periodo inicial y final deben ser diferentes."),
      need(input$period2_start != input$period2_end, "Comparación 2: El periodo inicial y final deben ser diferentes.")
    )
    
    p1_start <- input$period1_start
    p1_end <- input$period1_end
    p2_start <- input$period2_start
    p2_end <- input$period2_end
    
    # Create the comparison strings needed to filter the data
    # IMPORTANT: Assumes your CSV's 'comparacion' column stores strings like "P1 vs P2"
    comparison_str1 <- paste(p1_start, "vs", p1_end)
    comparison_str2 <- paste(p2_start, "vs", p2_end)
    
    # Filter the main data for the two selected comparison strings
    data_filtered <- orden_votantes_t_MCMC_procesado %>%
      filter(comparacion %in% c(comparison_str1, comparison_str2))
    
    # Check if data exists for the selected comparisons
    validate(
      need(nrow(data_filtered) > 0,
           paste("No hay datos de comparación precalculados para una o ambas selecciones:",
                 comparison_str1, "o", comparison_str2,
                 ". Verifique el archivo CSV o el script de preprocesamiento.")),
      # Check specifically if each required comparison was found after filtering
      need(comparison_str1 %in% unique(data_filtered$comparacion),
           paste("No se encontraron datos para la comparación:", comparison_str1)),
      need(comparison_str2 %in% unique(data_filtered$comparacion),
           paste("No se encontraron datos para la comparación:", comparison_str2))
    )
    
    # Separate data for each plot
    data_plot_1 <- data_filtered %>% filter(comparacion == comparison_str1)
    data_plot_2 <- data_filtered %>% filter(comparacion == comparison_str2)
    
    # --- Determine X-axis Ordering ---
    # Order based on the position in the START period of COMPARISON 1 (p1_start)
    # We need the 'posicion_inicial' values corresponding ONLY to comparisons *starting* with p1_start
    # We query the original processed data for this.
    base_order_data <- orden_votantes_t_MCMC_procesado %>%
      filter(Periodo1 == p1_start) %>%
      # If a voter appears multiple times for p1_start (compared to different second periods),
      # just take their first occurrence's position for ordering.
      distinct(Votante, .keep_all = TRUE) %>%
      select(Votante, base_posicion = posicion_inicial) # Rename to base_posicion for clarity
    
    validate(
      need(nrow(base_order_data) > 0, paste("No se pudo determinar el orden basado en el periodo:", p1_start, ". Verifique que existan comparaciones que comiencen con este periodo en los datos."))
    )
    
    # Join this base position back to the filtered plot data for ordering
    # Use inner_join to ensure only voters present in the base ordering data are plotted
    # (or left_join if you want to see voters even if their base position is missing, though they won't order correctly)
    data_plot_1 <- data_plot_1 %>% inner_join(base_order_data, by = "Votante")
    data_plot_2 <- data_plot_2 %>% inner_join(base_order_data, by = "Votante")
    
    # Final check after join
    validate(
      need(nrow(data_plot_1) > 0, paste("No se encontraron datos válidos para mostrar para:", comparison_str1, "después de aplicar el orden.")),
      need(nrow(data_plot_2) > 0, paste("No se encontraron datos válidos para mostrar para:", comparison_str2, "después de aplicar el orden.")),
      need(all(!is.na(data_plot_1$base_posicion)), "Error interno: No se pudieron determinar las posiciones base para el gráfico 1."),
      need(all(!is.na(data_plot_2$base_posicion)), "Error interno: No se pudieron determinar las posiciones base para el gráfico 2.")
    )
    
    # Return a list containing data for both plots and the comparison strings
    list(
      data1 = data_plot_1,
      data2 = data_plot_2,
      comp_str1 = comparison_str1,
      comp_str2 = comparison_str2,
      order_period = p1_start # Pass the period used for ordering for the x-axis label
    )
  })
  
  # --- Render Plot (output$combined_plot) ---
  # The renderPlot part needs only MINOR changes:
  # 1. Update the x-axis label in plot_2 to use the dynamic order_period
  # 2. Ensure plot titles use plot_data_list$comp_str1 and plot_data_list$comp_str2
  # The core ggplot calls using fct_reorder(Votante, base_posicion) are ALREADY correct.
  
  output$combined_plot <- renderPlot({
    
    plot_data_list <- plot_data() # Get the reactive data
    
    # --- Define common plot elements (scales, shapes, colors) ---
    # (Keep the existing definitions for common_y_scale, common_color_scale, etc.)
    # ... (previous common element definitions remain the same) ...
    common_y_scale <- scale_y_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, by = 0.5))
    common_color_scale <- scale_color_manual(
      name = "Dirección del Cambio",
      values = c("Izquierda" = "red", "Derecha" = "blue", "Sin cambio" = "grey"), # Added grey for no change
      na.translate = FALSE
    )
    common_shape_scale <- scale_shape_manual(
      name = "Significancia Estadística",
      values = c("Significativo (p < 0.05)" = 21, "No Significativo (p >= 0.05)" = 24),
      drop = FALSE
    )
    common_fill_scale <- scale_fill_manual(
      name = "Significancia Estadística",
      values = c("Significativo (p < 0.05)" = "white", "No Significativo (p >= 0.05)" = "black"),
      drop = FALSE
    )
    common_guides <- guides(
      shape = guide_legend(title = "Significancia Estadística"),
      fill = guide_legend(title = "Significancia Estadística"),
      color = guide_legend(title = "Dirección del Cambio")
    )
    
    # --- Create Plot 1 (Top) ---
    plot_1 <- ggplot(plot_data_list$data1,
                     # Use the base_posicion from the join for consistent ordering
                     aes(x = fct_reorder(Votante, base_posicion), y = posicion_inicial)) +
      geom_segment(
        aes(xend = fct_reorder(Votante, base_posicion), yend = posicion_final, color = direccion_cambio_factor),
        arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth = 0.8
      ) +
      geom_point(
        aes(shape = etiqueta_significancia_factor, fill = etiqueta_significancia_factor),
        size = 2.75, stroke = 0.5, color = "darkgrey"
      ) +
      common_y_scale +
      common_color_scale +
      common_shape_scale +
      common_fill_scale +
      common_guides +
      labs(
        title = plot_data_list$comp_str1, # Dynamic title
        x = NULL,
        y = "Posición Política"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2),
        panel.grid.minor.x = element_blank()
      )
    
    # --- Create Plot 2 (Bottom) ---
    plot_2 <- ggplot(plot_data_list$data2,
                     aes(x = fct_reorder(Votante, base_posicion), y = posicion_inicial)) +
      geom_segment(
        aes(xend = fct_reorder(Votante, base_posicion), yend = posicion_final, color = direccion_cambio_factor),
        arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth = 0.8
      ) +
      geom_point(
        aes(shape = etiqueta_significancia_factor, fill = etiqueta_significancia_factor),
        size = 2.50, stroke = 0.5, color = "darkgrey"
      ) +
      common_y_scale +
      common_color_scale +
      common_shape_scale +
      common_fill_scale +
      common_guides +
      labs(
        title = plot_data_list$comp_str2, # Dynamic title
        # *** MODIFIED X LABEL HERE ***
        x = paste("Votante (ordenado por posición inicial en periodo", plot_data_list$order_period, ")"),
        y = "Posición Política"
      ) +
      theme_minimal(base_size = 15) + # Larger base size for bottom plot labels
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2),
        panel.grid.minor.x = element_blank()
      )
    
    # --- Combine Plots ---
    # (Combination logic remains the same)
    plot_combinado <- plot_1 / plot_2 +
      plot_layout(guides = 'collect') +
      plot_annotation(
        title = "Cambio en la Posición Política de Convencionales",
        subtitle = "Comparación posición ideológica con W-Nominate, según ventanas de sesiones seleccionadas",
        theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face="bold"),
                      plot.subtitle = element_text(hjust = 0.5, size = 12))
      ) & # Use & to apply theme elements potentially to collected legends too
      theme(legend.position = 'bottom') # Ensure legend is collected at the bottom
    
    print(plot_combinado) # Display the combined plot
    
  })
  
}

# --- 4. Run the Application ---------------------------------------------------

shinyApp(ui = ui, server = server)

#shiny::runApp("04_ord_pleno_shiny.R")
