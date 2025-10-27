# --- 0. Load Libraries ---
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(patchwork)
library(forcats) 
library(data.table)

# --- 1. Load and Prepare Data -------------------------------------------------

datos_comparacion <- readRDS("ideological-scaling-files/03_comparacion_t_MCMC_vs_WNOMINATE.rds")
setDT(datos_comparacion) 

# Crear las columnas necesarias para el gráfico
datos_comparacion_procesados <- datos_comparacion[, .(
  legislador, Periodo,
  posicion_wnominate = pos_ideol_wn, 
  posicion_mcmc = pos_ideol_mcmc,    
  dif_wn_mcmc = dif_pos_wn_mcmc,  # Diferencia numérica (WN - MCMC)
  p_valor = p_value
)][, ':='( # Usar := para añadir columnas eficientemente en data.table
  direccion_comparacion = fcase(
    dif_wn_mcmc > 0, "WN a la Derecha de MCMC",
    dif_wn_mcmc < 0, "WN a la Izquierda de MCMC",
    default = "Sin diferencia"
  ),
  # Determinar significancia para forma/relleno del punto
  significativo = p_valor < 0.05,
  etiqueta_significancia = fifelse(p_valor < 0.05, "Significativo (p < 0.05)", "No Significativo (p >= 0.05)")
)][, ':='( # Crear factores
  direccion_comparacion_factor = factor(direccion_comparacion, levels = c("WN a la Izquierda de MCMC", "WN a la Derecha de MCMC", "Sin diferencia")),
  etiqueta_significancia_factor = factor(etiqueta_significancia, levels = c("Significativo (p < 0.05)", "No Significativo (p >= 0.05)"))
)]

# Verificar columnas creadas
head(datos_comparacion_procesados)
print(table(datos_comparacion_procesados$direccion_comparacion))

# Get unique periods available
available_periods <- unique(datos_comparacion_procesados$Periodo)

# --- 2. Define UI -------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Comparación de Ordenamientos Políticos: WNOMINATE vs IDEAL — CC 2021-2022"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Seleccionar Período para cada Gráfico"),
      p("Cada gráfico muestra la comparación entre WNOMINATE e IDEAL para el período de sesiones seleccionado."),
      
      br(), 
      
      # --- Input for Plot 1 (Top) ---
      h5("Gráfico Superior:"),
      selectInput("periodo_plot_1",
                  "Período:",
                  choices = available_periods,
                  selected = available_periods[1]), 
      
      br(), 
      
      # --- Input for Plot 2 (Bottom) ---
      h5("Gráfico Inferior:"),
      selectInput("periodo_plot_2",
                  "Período:",
                  choices = available_periods,
                  selected = if(length(available_periods) > 1) available_periods[2] else available_periods[1]), 
      
      hr(),
      p(strong("El orden del Eje X"), " se basa en la posición ", strong("WNOMINATE"), " de los convencionales en el período seleccionado para el ", strong("gráfico superior.")),
      p(strong("Los puntos"), " muestran la posición estimada por ", strong("WNOMINATE.")),
      p(strong("Las flechas"), " van desde la posición WNOMINATE hacia la posición IDEAL."),
      p(strong("Color flecha:"), "Azul si WN está a la derecha de IDEAL; Rojo si WN está a la izquierda."),
      p(strong("Forma/Relleno del punto:"), " indica si la diferencia entre métodos es estadísticamente significativa según test-t (Círculo Blanco: Significativo p<0.05, Triángulo Negro: No Significativo).")
    ),
    
    mainPanel(
      br(), 
      
      # Output for the combined plot
      plotOutput("combined_plot", height = "700px") # Adjust height as needed
    )
  )
)

# --- 3. Define Server Logic ---------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive expression to generate the plot data based on selections
  plot_data <- reactive({
    # Ensure both inputs are selected
    req(input$periodo_plot_1, input$periodo_plot_2)
    
    periodo1 <- input$periodo_plot_1
    periodo2 <- input$periodo_plot_2
    
    # Filter the main data for the two selected periods
    data_filtered <- datos_comparacion_procesados[Periodo %in% c(periodo1, periodo2)]
    
    # Check if data exists for the selected periods
    validate(
      need(nrow(data_filtered) > 0, "No hay datos para los períodos seleccionados."),
      need(periodo1 %in% unique(data_filtered$Periodo), paste("No se encontraron datos para el período:", periodo1)),
      need(periodo2 %in% unique(data_filtered$Periodo), paste("No se encontraron datos para el período:", periodo2))
    )
    
    # Separate data for each plot
    data_plot_1 <- data_filtered[Periodo == periodo1]
    data_plot_2 <- data_filtered[Periodo == periodo2]
    
    # --- Determine X-axis Ordering ---
    # based on the WNOMINATE position (in the period selected for PLOT 1)
    base_order_data <- data_plot_1[, .(legislador, base_posicion_wn = posicion_wnominate)]
    
    # Ensure base_order_data is not empty
    validate(
      need(nrow(base_order_data) > 0, paste("No se encontraron datos de WNOMINATE para el período:", periodo1, "para establecer el orden."))
    )
    
    # Join this base position back to the plot data using data.table merge syntax
    setkey(data_plot_1, legislador)
    setkey(data_plot_2, legislador)
    setkey(base_order_data, legislador)
    
    data_plot_1 <- merge(data_plot_1, base_order_data, all.x = FALSE) 
    data_plot_2 <- merge(data_plot_2, base_order_data, all.x = FALSE) 
    
    # Final check after join
    validate(
      need(nrow(data_plot_1) > 0, paste("No se encontraron datos válidos para mostrar para el período:", periodo1, "después de aplicar el orden.")),
      need(nrow(data_plot_2) > 0, paste("No se encontraron datos válidos para mostrar para el período:", periodo2, "después de aplicar el orden.")),
      need(all(!is.na(data_plot_1$base_posicion_wn)), "Error interno: No se pudieron determinar las posiciones base para el gráfico 1."),
      need(all(!is.na(data_plot_2$base_posicion_wn)), "Error interno: No se pudieron determinar las posiciones base para el gráfico 2.")
    )
    
    # Return a list containing data for both plots and the period names
    list(
      data1 = data_plot_1,
      data2 = data_plot_2,
      periodo1 = periodo1,
      periodo2 = periodo2
    )
  })
  
  # --- Render Plot (output$combined_plot) ---
  output$combined_plot <- renderPlot({
    
    plot_data_list <- plot_data() # Get the reactive data
    
    # --- Define common plot elements (scales, shapes, colors) ---
    common_y_scale <- scale_y_continuous(limits = c(-1.1, 1.1), breaks = seq(-1, 1, by = 0.5))
    common_color_scale <- scale_color_manual(
      name = "Diferencia WN vs IDEAL",
      values = c("WN a la Izquierda de MCMC" = "red", 
                 "WN a la Derecha de MCMC" = "blue", 
                 "Sin diferencia" = "grey"), 
      na.translate = FALSE
    )
    common_shape_scale <- scale_shape_manual(
      name = "Significancia (Test-t)",
      values = c("Significativo (p < 0.05)" = 21, "No Significativo (p >= 0.05)" = 24),
      drop = FALSE # Keep all levels in legend even if not present in current data
    )
    # Relleno: Significativo=Blanco, No Significativo=Negro (como en ejemplo)
    common_fill_scale <- scale_fill_manual(
      name = "Significancia (Test-t)",
      values = c("Significativo (p < 0.05)" = "white", "No Significativo (p >= 0.05)" = "black"),
      drop = FALSE
    )
    common_guides <- guides(
      shape = guide_legend(title = "Significancia (Test-t)"),
      fill = guide_legend(title = "Significancia (Test-t)"),
      color = guide_legend(title = "Diferencia WN vs IDEAL")
    )
    
    # --- Plot 1 ---
    plot_1 <- ggplot(plot_data_list$data1,
                     # Order by base_posicion_wn from periodo1
                     # Y position of point is posicion_wnominate
                     aes(x = fct_reorder(legislador, base_posicion_wn), y = posicion_wnominate)) +
      geom_segment(
        # Arrow starts at WN, ends at MCMC. Color indicates relative position.
        aes(xend = fct_reorder(legislador, base_posicion_wn), 
            yend = posicion_mcmc, color = direccion_comparacion_factor),
        arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth = 0.8
      ) +
      geom_point(
        # Point at WN position. Shape/fill show significance of the difference.
        aes(shape = etiqueta_significancia_factor, fill = etiqueta_significancia_factor),
        size = 2.75, stroke = 0.5, color = "darkgrey" # Point outline color
      ) +
      common_y_scale +
      common_color_scale +
      common_shape_scale +
      common_fill_scale +
      common_guides +
      labs(
        title = paste("Comparación WN vs IDEAL - Período:", plot_data_list$periodo1), # Dynamic title
        x = NULL, # No x-axis label for top plot
        y = "Posición Política (Rescaled)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        axis.text.x = element_blank(), # Hide x-axis text
        axis.ticks.x = element_blank(), # Hide x-axis ticks
        legend.position = "none",       # Hide legend for individual plot
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2),
        panel.grid.minor.x = element_blank()
      )
    
    # --- Plot 2 ---
    plot_2 <- ggplot(plot_data_list$data2,
                     # Use SAME ordering based on base_posicion_wn from plot 1
                     aes(x = fct_reorder(legislador, base_posicion_wn), y = posicion_wnominate)) +
      geom_segment(
        aes(xend = fct_reorder(legislador, base_posicion_wn), 
            yend = posicion_mcmc, color = direccion_comparacion_factor),
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
        title = paste("Comparación WN vs IDEAL - Período:", plot_data_list$periodo2), # Dynamic title
        x = paste("Convencional (ordenado por posición WNOMINATE en período", plot_data_list$periodo1, ")"), # Dynamic X label
        y = "Posición Política (Rescaled)"
      ) +
      theme_minimal(base_size = 15) + # Larger base size for bottom plot labels
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), # Keep x-axis text
        legend.position = "none", # Hide legend for individual plot
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2),
        panel.grid.minor.x = element_blank()
      )
    
    # --- Combine Plots ---
    plot_combinado <- plot_1 / plot_2 +
      plot_layout(guides = 'collect') + # Collect legends from both plots
      plot_annotation(
        title = "Comparación de Posiciones Estimadas: WNOMINATE vs IDEAL",
        subtitle = paste("Períodos seleccionados:", plot_data_list$periodo1, "(arriba) y", plot_data_list$periodo2, "(abajo)"),
        theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face="bold"),
                      plot.subtitle = element_text(hjust = 0.5, size = 12))
      ) & # Apply theme to collected legends as well
      theme(legend.position = 'bottom', legend.box = 'horizontal') # Place collected legend at the bottom
    
    print(plot_combinado) # Display the combined plot
    
  })
  
}

# --- 4. Run the Application ---------------------------------------------------
shinyApp(ui = ui, server = server)

# shiny::runApp("04_ord_pleno_shiny_MCMC_vs_WNOM.R") 
