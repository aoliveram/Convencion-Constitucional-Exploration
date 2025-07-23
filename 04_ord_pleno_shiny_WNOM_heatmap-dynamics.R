# --- 0. Load Libraries ---
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr) # for separate()
library(stringr)
library(forcats)
library(viridis) # color palettes

# --- 1. Load and Prepare Data -------------------------------------------------
tryCatch({
  orden_votantes_t_raw <- readRDS("scripts - files/03_orden_votantes_t.rds")
}, error = function(e) {
  stop("Error loading scripts - files/03_orden_votantes_t.rds. Make sure the file exists. Original error: ", e$message)
})

# --- Data Preparation Logic ---

# 1. Extract all "initial" positions from the raw data
data_inicial <- orden_votantes_t_raw %>%
  separate(comparacion, into = c("Periodo", "Periodo_end"), sep = " vs ", remove = FALSE) %>%
  select(Votante, Periodo, posicion_continua = pos_ideol_inicial)

# 2. Extract and calculate all "final" positions from the raw data
data_final <- orden_votantes_t_raw %>%
  separate(comparacion, into = c("Periodo_start", "Periodo"), sep = " vs ", remove = FALSE) %>%
  mutate(posicion_continua = pos_ideol_inicial + dif_media) %>% # Calculate the position in the final period
  select(Votante, Periodo, posicion_continua)

# 3. Combine and get a single, unique position for each voter-period pair
positions_data <- bind_rows(data_inicial, data_final) %>%
  distinct(Votante, Periodo, .keep_all = TRUE)

# 4. Get the list of all unique periods and sort them
periodos_unicos <- sort(unique(positions_data$Periodo))

# 5. Build the final, full data frame with all metrics
full_data <- positions_data %>%
  # Ensure Periodo is an ordered factor for correct chronological sorting and plotting
  mutate(Periodo = factor(Periodo, levels = periodos_unicos, ordered = TRUE)) %>%
  # Calculate ordinal position (rank) within each period block
  group_by(Periodo) %>%
  mutate(posicion_ordinal = rank(-posicion_continua, ties.method = "first")) %>%
  ungroup() %>%
  # Arrange by Votante and Period to correctly calculate differences vs. the previous period
  arrange(Votante, Periodo) %>%
  group_by(Votante) %>%
  mutate(
    # Calculate difference vs. previous period using lag()
    diferencia_continua = posicion_continua - lag(posicion_continua),
    diferencia_ordinal = posicion_ordinal - lag(posicion_ordinal)
  ) %>%
  # Replace NA (for the first period) with 0, as there's no change from a prior state
  mutate(
    diferencia_continua = ifelse(is.na(diferencia_continua), 0, diferencia_continua),
    diferencia_ordinal = ifelse(is.na(diferencia_ordinal), 0, diferencia_ordinal)
  ) %>%
  ungroup()

# 6. Get the definitive, unique order of Votantes based on their position in the FIRST period
votante_order <- full_data %>%
  filter(Periodo == periodos_unicos[1]) %>%
  arrange(posicion_continua) %>% # Order from left (-1) to right (1)
  pull(Votante) # This will now be unique due to the corrected 'positions_data'

# 7. Apply this fixed order to the main data frame's Votante factor levels
# The `rev()` call places left-wing (more negative) at the top of the heatmap
full_data$Votante <- factor(full_data$Votante, levels = rev(votante_order))

# Filter out any voters who might not have been present in the first period (and thus are NA in the factor)
full_data <- full_data %>%
  filter(!is.na(Votante))

# 8. Create a separate, alphabetically sorted list for the dropdown input ---
available_members_sorted <- sort(unique(as.character(full_data$Votante)))

# --- 2. Define UI -------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Análisis de Dinámica Política en la Convención Constitucional (2021-2022)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controles de Visualización"),
      
      selectInput("tipo_ordenamiento",
                  "1. Seleccione el tipo de ordenamiento:",
                  choices = c("Continuo (-1 a 1)" = "continuo", "Ordinal (1 a 154)" = "ordinal"),
                  selected = "continuo"),
      
      # --- MODIFIED: Use the alphabetically sorted list for choices ---
      selectizeInput("convencionales_seleccionados",
                     "2. Seleccione convencionales para el gráfico de dinámica:",
                     choices = available_members_sorted, # Uses the new sorted list
                     selected = c("Marinovic, Teresa", "Zuñiga, Luis Arturo"), # Default selection
                     multiple = TRUE,
                     options = list(placeholder = 'Escriba un nombre...')),
      
      hr(),
      p(strong("Heatmap de Posicionamiento (Gráfico Superior):")),
      p("Muestra la posición ideológica de cada convencional a lo largo del tiempo. ",
        "El eje Y está ordenado de izquierda (arriba) a derecha (abajo) según la posición en el primer bloque (01-15). ",
        "El color rojo indica una posición de izquierda, el azul de derecha y el blanco de centro."),
      
      p(strong("Gráfico de Dinámica (Gráfico Inferior):")),
      p("Muestra el cambio en la posición de los convencionales seleccionados. ",
        "El eje Y representa la diferencia de posición con respecto al bloque de sesiones inmediatamente anterior. Un valor positivo indica un movimiento hacia la derecha; uno negativo, hacia la izquierda.")
    ),
    
    mainPanel(
      h3("Heatmap de Posicionamiento Político por Bloque de Sesiones"),
      plotOutput("heatmap_plot", height = "600px"),
      hr(),
      h3("Dinámica de Cambio en el Posicionamiento"),
      plotOutput("dynamics_plot", height = "400px")
    )
  )
)

# --- 3. Define Server Logic ---------------------------------------------------
server <- function(input, output) {
  
  # --- 3.1. Heatmap Plot ---
  output$heatmap_plot <- renderPlot({
    
    if (input$tipo_ordenamiento == "continuo") {
      fill_var <- "posicion_continua"
      midpoint_color <- 0
      color_limits <- c(-1, 1)
      legend_title <- "Posición Continua"
    } else {
      fill_var <- "posicion_ordinal"
      midpoint_color <- 77.5 # Midpoint for 1-154 positions
      color_limits <- c(1, 154)
      legend_title <- "Posición Ordinal"
    }
    
    ggplot(full_data, aes(x = Periodo, y = Votante, fill = .data[[fill_var]])) +
      geom_tile(color = "white", lwd = 0.2) +
      scale_fill_gradient2(
        low = "red",
        mid = "white",
        high = "blue",
        midpoint = midpoint_color,
        limit = color_limits,
        name = legend_title
      ) +
      labs(
        x = "Bloque de Sesiones de Votación",
        y = "Convencionales (Ordenados por posición en el bloque 01-15)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        # --- MODIFIED: Font size for Y-axis labels is now 6 ---
        axis.text.y = element_text(size = 6),
        legend.position = "right",
        panel.grid = element_blank()
      )
  })
  
  # --- 3.2. Dynamics Line Plot ---
  output$dynamics_plot <- renderPlot({
    
    validate(
      need(input$convencionales_seleccionados, "Por favor, seleccione al menos un convencional en el panel de la izquierda.")
    )
    
    dynamics_data <- full_data %>%
      filter(Votante %in% input$convencionales_seleccionados)
    
    if (input$tipo_ordenamiento == "continuo") {
      diff_var <- "diferencia_continua"
      y_limits <- c(-1, 1)
      y_label <- "Diferencia de Posición Continua (vs. bloque anterior)"
    } else {
      diff_var <- "diferencia_ordinal"
      y_limits <- c(-50, 50)
      y_label <- "Diferencia de Posición Ordinal (vs. bloque anterior)"
    }
    
    ggplot(dynamics_data, aes(x = Periodo, y = .data[[diff_var]], group = Votante, color = Votante)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_color_viridis_d(name = "Convencional") +
      scale_y_continuous(limits = y_limits) +
      labs(
        x = "Bloque de Sesiones de Votación",
        y = y_label,
        title = "Cambio en Posición Respecto al Bloque Anterior"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
}


# --- 4. Run the Application ---------------------------------------------------
shinyApp(ui = ui, server = server)

