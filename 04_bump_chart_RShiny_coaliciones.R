# --- 1. Load and Prepare Data ------------------------------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(cowplot)
  library(readr)
  library(stringr)
})

# Rutas por defecto (ajusta si las cambias)
ORDEN_T_PATH_RDS <- "ideological-scaling-files/03_orden_votantes_t.rds"
COAL_RDS_PATH    <- "ideological-scaling-files/coaliciones_convencionales.rds"

# Abreviaciones y paleta (claves = abreviaciones)
abbr_map <- c(
  "Coordinadora Constituyente Plurinacional y Popular" = "CCPP",
  "Escaños Reservados" = "Esc. Res.",
  "Frente Amplio" = "FA",
  "Colectivo Socialista" = "Col. Soc.",
  "Movimientos Sociales Constituyentes" = "Mov. SC",
  "Pueblo Constituyente" = "Pueblo. C.",
  "Independientes No Neutrales" = "INN",
  "Chile Digno" = "Chile Digno",
  "Colectivo del Apruebo" = "Col. A",
  "Sin Grupo" = "Sin. G.",
  "Chile Libre" = "Chile Libre",
  "Unidos por Chile" = "Un. x Ch.",
  "Chile Unido" = "Chile Unido",
  "Independientes RN-Evópoli" = "RN-Evo",
  "Vamos por Chile" = "V. x C."
)

cols_coal <- c(
  "CCPP"        = "#1f77b4",
  "Esc. Res."   = "#ff7f0e",
  "FA"          = "#2ca02c",
  "Col. Soc."   = "#d62728",
  "Mov. SC"     = "#9467bd",
  "Pueblo. C."  = "#8c564b",
  "INN"         = "#e377c2",
  "Chile Digno" = "#7f7f7f",
  "Col. A"      = "#bcbd22",
  "Sin. G."     = "#17becf",
  "Chile Libre" = "#1b9e77",
  "Un. x Ch."   = "#d95f02",
  "Chile Unido" = "#7570b3",
  "RN-Evo"      = "#e7298a",
  "V. x C."     = "#66a61e"
)

niveles_periodo <- c("01-15","16-21","22-37","56-75","76-99","100-106")

# Helpers ---------------------------------------------------------------------
load_coaliciones <- function() {
  if (file.exists(COAL_RDS_PATH)) {
    x <- readRDS(COAL_RDS_PATH)
    validate_coal(x)
    return(x)
  }
  stop(paste0(
    "No se encontró 'coaliciones_convencionales'.\n",
    "Guarda tu objeto (nombre, coalicion) como RDS en:\n  - ", COAL_RDS_PATH))
}

validate_coal <- function(tbl) {
  req <- c("nombre","coalicion")
  if (!all(req %in% names(tbl))) {
    stop("El mapeo de coaliciones debe tener columnas: 'nombre' y 'coalicion'.")
  }
  invisible(TRUE)
}

load_orden_t <- function() {
  if (!file.exists(ORDEN_T_PATH_RDS)) {
    stop("No se encontró 'ideological-scaling-files/03_orden_votantes_t.rds'.")
  }
  readRDS(ORDEN_T_PATH_RDS)
}

prep_long <- function(orden_t_df) {
  # Construye tabla larga con Periodo1/pos_ideol_inicial y Periodo2/pos_ideol_final (incluye 100-106)
  bind_rows(
    orden_t_df %>% transmute(Votante, Periodo = Periodo1, posicion_ideologica = pos_ideol_inicial),
    orden_t_df %>% transmute(Votante, Periodo = Periodo2, posicion_ideologica = pos_ideol_final)
  ) %>%
    group_by(Votante, Periodo) %>%
    summarise(posicion_ideologica = mean(posicion_ideologica, na.rm = TRUE), .groups = "drop")
}

# Carga base
coaliciones_convencionales <- load_coaliciones()
orden_t_df  <- load_orden_t()
orden_t_slim <- prep_long(orden_t_df)

# Join con coaliciones, y resumen por coalición / periodo
base_join <- orden_t_slim %>%
  left_join(coaliciones_convencionales, by = c("Votante" = "nombre"))

promedios_coal_periodo <- base_join %>%
  filter(!is.na(coalicion)) %>%
  group_by(coalicion, Periodo) %>%
  summarise(
    posicion_media = mean(posicion_ideologica, na.rm = TRUE),
    sd_pos         = sd(posicion_ideologica, na.rm = TRUE),
    n              = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Periodo = factor(Periodo, levels = niveles_periodo),
    Periodo_num = as.numeric(Periodo),
    coal_abbr = dplyr::recode(coalicion, !!!abbr_map, .default = coalicion)
  ) %>%
  arrange(Periodo)

# Para UI: opciones limpias y ordenadas
coal_opts <- promedios_coal_periodo %>%
  distinct(coalicion, coal_abbr) %>%
  arrange(coal_abbr)

# --- 2. Define UI ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Dinámica de posiciones: coaliciones y convencionales"),
  sidebarLayout(
    sidebarPanel(
      # Sección 1: controles
      h4("1) Selección"),
      helpText("Elige coaliciones para el primer gráfico y convencionales de una coalición para el segundo."),
      selectizeInput(
        inputId = "coalitions_selected",
        label   = "Coaliciones para 'bump' por coalición:",
        choices = setNames(coal_opts$coalicion, coal_opts$coal_abbr),
        selected = coal_opts$coalicion, multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      selectizeInput(
        inputId = "coalition_for_people",
        label   = "Coalición para 'bump' de convencionales:",
        choices = setNames(coal_opts$coalicion, coal_opts$coal_abbr),
        selected = if (nrow(coal_opts)>0) coal_opts$coalicion[1] else NULL,
        multiple = FALSE
      ),
      uiOutput("people_picker"),
      sliderInput(
        inputId = "ribbon_mult",
        label   = "Ancho de banda (multiplicador de DE):",
        min = 0, max = 1, value = 0.5, step = 0.01
      ),
      hr(),
      helpText("Colores y abreviaciones fijados para coherencia visual con tus gráficos.")
    ),
    mainPanel(
      # Sección 2: bump por coalición
      h4("2) Bump chart por coaliciones"),
      plotOutput("plot_coal", height = "520px"),
      tags$br(),
      # Sección 3: bump por convencionales
      h4("3) Bump chart por convencionales (coalición seleccionada)"),
      plotOutput("plot_people", height = "520px")
    )
  )
)

# --- 3. Define Server Logic --------------------------------------------------
server <- function(input, output, session) {
  
  # Actualiza la lista de convencionales al cambiar la coalición
  choices_people <- reactive({
    req(input$coalition_for_people)
    base_join %>%
      filter(coalicion == input$coalition_for_people) %>%
      distinct(Votante) %>%
      arrange(Votante) %>%
      pull(Votante)
  })
  
  output$people_picker <- renderUI({
    selectizeInput(
      inputId = "people_selected",
      label = "Convencionales:",
      choices = choices_people(),
      selected = head(choices_people(), 8),
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })
  
  # Datos filtrados para el gráfico por coalición
  coal_df <- reactive({
    req(input$coalitions_selected, length(input$coalitions_selected) > 0)
    promedios_coal_periodo %>%
      filter(coalicion %in% input$coalitions_selected) %>%
      mutate(
        coal_abbr = dplyr::recode(coalicion, !!!abbr_map, .default = coalicion),
        Periodo   = factor(Periodo, levels = niveles_periodo),
        Periodo_num = as.numeric(Periodo)
      )
  })
  
  # Panel derecho con últimos promedios
  right_labels <- reactive({
    coal_df() %>%
      group_by(coal_abbr) %>%
      slice_max(Periodo_num, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      arrange(desc(posicion_media)) %>%
      transmute(coal_abbr, posicion_media) %>%
      mutate(label = sprintf("%s    %.3f", coal_abbr, posicion_media))
  })
  
  output$plot_coal <- renderPlot({
    df <- coal_df()
    req(nrow(df) > 0)
    k <- input$ribbon_mult
    # gráfico principal
    p_main <- ggplot(df, aes(x = Periodo_num, y = posicion_media, group = coal_abbr, color = coal_abbr)) +
      geom_ribbon(aes(ymin = posicion_media - k * sd_pos,
                      ymax = posicion_media + k * sd_pos,
                      fill = coal_abbr),
                  alpha = 0.18, color = NA) +
      geom_line(size = 0.9) +
      geom_point(size = 1.8) +
      scale_x_continuous(breaks = seq_along(niveles_periodo), labels = niveles_periodo) +
      scale_color_manual(values = cols_coal, guide = "none") +
      scale_fill_manual(values = cols_coal, guide = "none") +
      labs(x = "Bloque de sesiones", y = "Posición ideológica promedio",
           title = "Evolución de la posición promedio por coalición",
           subtitle = paste0("Banda ±", k, "·DE (W-NOMINATE 1D)")) +
      ylim(-1, 1) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 6))
      )
    
    # panel derecho de etiquetas
    labs_df <- right_labels()
    p_right <- ggplot(labs_df, aes(x = 0, y = reorder(coal_abbr, posicion_media))) +
      geom_text(aes(label = label, color = coal_abbr), hjust = 0, size = 3.3) +
      scale_color_manual(values = cols_coal, guide = "none") +
      scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      labs(x = NULL, y = NULL) +
      theme_void() +
      theme(plot.margin = margin(10, 10, 10, 10))
    
    cowplot::plot_grid(p_main, p_right, ncol = 2, rel_widths = c(4, 1.4))
  })
  
  # Datos para gráfico de convencionales
  people_df <- reactive({
    req(input$coalition_for_people, input$people_selected)
    base_join %>%
      filter(coalicion == input$coalition_for_people,
             Votante %in% input$people_selected) %>%
      mutate(
        Periodo = factor(Periodo, levels = niveles_periodo),
        Periodo_num = as.numeric(Periodo),
        coal_abbr = dplyr::recode(coalicion, !!!abbr_map, .default = coalicion)
      ) %>%
      arrange(Votante, Periodo_num)
  })
  
  output$plot_people <- renderPlot({
    df <- people_df()
    req(nrow(df) > 0)
    # color base de la coalición seleccionada
    coal_lab <- dplyr::recode(unique(df$coalicion), !!!abbr_map, .default = unique(df$coalicion))[1]
    base_col <- cols_coal[[coal_lab]]
    if (is.null(base_col)) base_col <- "black"
    
    ggplot(df, aes(x = Periodo_num, y = posicion_ideologica, group = Votante)) +
      geom_line(color = base_col, size = 0.8, alpha = 0.9) +
      geom_point(color = base_col, size = 1.8, alpha = 0.9) +
      scale_x_continuous(breaks = seq_along(niveles_periodo), labels = niveles_periodo) +
      labs(x = "Bloque de sesiones", y = "Posición ideológica",
           title = paste0("Evolución por convencional (", coal_lab, ")"),
           subtitle = paste0(length(unique(df$Votante)), " convencionales seleccionados")) +
      ylim(-1, 1) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 6))
      )
  })
}

# --- 4. Run the Application ---------------------------------------------------
shinyApp(ui = ui, server = server)
