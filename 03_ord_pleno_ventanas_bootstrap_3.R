library(readr)
library(data.table)
library(dplyr)
library(ggplot2)

normalizar_nombres <- function(texto) {
  texto_limpio <- iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT")
  return(texto_limpio)
}

# Reescalamos dentro de [-1,1]
reescalar <- function(vector_original) {
  min_original <- min(vector_original)
  max_original <- max(vector_original)
  a <- -1
  b <- 1
  vector_reescalado <- ((vector_original - min_original) / (max_original - min_original)) * (b - a) + a
  return(vector_reescalado)
}

#------------------------------------------------------------------------------
# ¿Son estadísticamente distintos?
#------------------------------------------------------------------------------

ordenamiento_1D_WNOM_al_14ago2021_bootstrap <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_al-14ago2021_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)
ordenamiento_1D_WNOM_al_14ago2021_bootstrap <- ordenamiento_1D_WNOM_al_14ago2021_bootstrap[, 3] %>% data.frame()

ordenamiento_1D_WNOM_22_37_bootstrap <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_22-37_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)
ordenamiento_1D_WNOM_22_37_bootstrap <- ordenamiento_1D_WNOM_22_37_bootstrap[, 3] %>% data.frame()

# Combina los dos data frames en uno solo

votantes

ordenamientos_14ago2021_22_37 <- data.frame(
  Votante = rep(votantes, times = 200),  # ID del votante repetido 200 veces
  "01-15" = reescalar(ordenamiento_1D_WNOM_al_14ago2021_bootstrap$.),
  "22-37" = reescalar(ordenamiento_1D_WNOM_22_37_bootstrap$.) 
)

orden_votantes_t <- ordenamientos_14ago2021_22_37 %>%
  group_by(Votante) %>%
  summarize(
    p_valor = t.test(X01.15, X22.37, paired = TRUE)$p.value
  )

print(orden_votantes_t, n=41)

# Filtra los datos para incluir solo los votantes con p_valor > 0.05
votantes_sin_cambio_ord <- orden_votantes_t[orden_votantes_t$p_valor > 0.05, ]

ggplot(orden_votantes_t, aes(x = Votante, y = p_valor, fill = -p_valor)) +
  geom_bar(width = 0.9, stat = "identity", position = position_dodge()) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(y = "p-value") +
  ggtitle("Contraste de hipótesis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 15)) +
  scale_x_discrete(breaks = votantes_sin_cambio_ord$Votante) +
  coord_cartesian(ylim = c(0, 0.085)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------ General

# Archivo de ordenamiento primero
ordenamiento_1D_WNOM_al_14ago2021_bootstrap <- read.csv(
  "Pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_al-14ago2021_bootstrap.csv",
  stringsAsFactors = FALSE,  # Preserve string formatting
  encoding = "UTF-8"         # Maintain special characters
)
ordenamiento_1D_WNOM_al_14ago2021_bootstrap <- ordenamiento_1D_WNOM_al_14ago2021_bootstrap[, 3] %>% data.frame()

# Lista de archivos CSV a leer
archivos_csv <- c("ordenamiento_1D_WNOM_22-37_bootstrap.csv",
                  "ordenamiento_1D_WNOM_56-75_bootstrap.csv", 
                  "ordenamiento_1D_WNOM_76-99_bootstrap.csv",
                  "ordenamiento_1D_WNOM_100-106_bootstrap.csv")

# Función para leer y procesar cada archivo
leer_csv <- function(archivo) {
  df <- read.csv(paste0("Pleno/ordenamientos_pleno/", archivo), 
                 stringsAsFactors = FALSE, 
                 encoding = "UTF-8")
  df <- df[, 3] %>% data.frame()
  colnames(df) <- gsub("_bootstrap.csv", "", gsub("ordenamiento_1D_WNOM_", "", archivo))
  return(df)
}

# Leer todos los archivos
lista_dfs <- lapply(archivos_csv, leer_csv)

# Combinar todos los dataframes
ordenamientos_completos <- cbind(
  Votante = rep(votantes, times = 200),
  "01-15" = reescalar(ordenamiento_1D_WNOM_al_14ago2021_bootstrap$.),
  do.call(cbind, lista_dfs)
)

# Función para realizar t-test entre dos columnas
realizar_t_test <- function(df, col1, col2) {
  t_result <- t.test(df[[col1]], df[[col2]], paired = TRUE)
  data.frame(
    comparacion = paste(col1, "vs", col2),
    p_valor = t_result$p.value,
    diferencia_media = t_result$estimate
  )
}

# Obtener todas las combinaciones posibles de columnas
columnas_numericas <- names(ordenamientos_completos)[-1]
combinaciones <- combn(columnas_numericas, 2, simplify = FALSE)

# Realizar t-test para todas las combinaciones
orden_votantes_t <- ordenamientos_completos %>%
  group_by(Votante) %>%
  do(bind_rows(lapply(combinaciones, function(combo) realizar_t_test(., combo[1], combo[2]))))

library(ggplot2)
library(gridExtra)

# Función para crear un plot individual
crear_plot <- function(data, comparacion) {
  votantes_sin_cambio <- data %>% 
    filter(comparacion == !!comparacion, p_valor > 0.05) %>% 
    pull(Votante)
  
  ggplot(data %>% filter(comparacion == !!comparacion), 
         aes(x = Votante, y = p_valor, fill = -p_valor)) +
    geom_bar(width = 0.9, stat = "identity", position = position_dodge()) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 0.5) +
    labs(y = "p-value", title = comparacion) +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
    scale_x_discrete(breaks = votantes_sin_cambio) +
    coord_cartesian(ylim = c(0, 0.085)) +
    theme(legend.position = "none")
}

# Crear lista de plots
lista_plots <- unique(orden_votantes_t$comparacion) %>%
  lapply(function(comp) crear_plot(orden_votantes_t, comp))

# Determinar el número de filas y columnas para el arreglo
n_plots <- length(lista_plots)
n_cols <- ceiling(sqrt(n_plots))
n_rows <- ceiling(n_plots / n_cols)

# Crear el arreglo de plots
arreglo_plots <- do.call(grid.arrange, c(lista_plots, ncol = n_cols, nrow = n_rows))

# Guardar el arreglo de plots
ggsave("plots/matriz_comparaciones_sesiones.png", arreglo_plots, width = 20, height = 20, units = "in", dpi = 300)

# --------------------- Plot alternativo 
library(ggplot2)
library(patchwork)
library(dplyr)
library(purrr)

# Define the order of session windows
ventanas <- c("01-15", "22-37", "56-75", "76-99", "100-106")

# Function to create an individual plot
crear_plot <- function(data, x_ventana, y_ventana) {
  # Avoid redundant comparisons (diagonal and upper triangle)
  if (x_ventana == y_ventana || which(ventanas == x_ventana) > which(ventanas == y_ventana)) {
    return(ggplot() + theme_void())
  }
  
  comparacion <- paste(x_ventana, "vs", y_ventana)
  
  # Filter data for this comparison
  datos_filtrados <- data %>% filter(comparacion == !!comparacion)
  
  # Get names of voters with p > 0.05
  votantes_sin_cambio <- datos_filtrados %>%
    filter(p_valor > 0.05) %>%
    pull(Votante)
  
  # Create the plot
  ggplot(datos_filtrados, 
         aes(x = Votante, y = p_valor, fill = -p_valor)) +
    geom_bar(width = 0.9, stat = "identity", position = position_dodge()) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 0.5) +
    labs(title = comparacion) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6),
      legend.position = "none"
    ) +
    scale_x_discrete(breaks = votantes_sin_cambio) +
    coord_cartesian(ylim = c(0, 0.085))
}

# Create a grid of combinations in the correct order (row vs column)
plots <- expand.grid(y = ventanas, x = ventanas) %>%
  arrange(match(x, ventanas), match(y, ventanas)) %>% # Correct row-column logic
  mutate(plot = pmap(list(x, y), ~ crear_plot(orden_votantes_t, ..1, ..2)))

# Combine the plots into a grid
grilla_plots <- wrap_plots(plots$plot, ncol = length(ventanas)) +
  plot_layout(guides = 'collect') &
  theme(plot.margin = margin(5, 5, 5, 5))

# Add axis labels and general title
grilla_final <- grilla_plots +
  plot_annotation(
    title = "Comparación de posiciones políticas entre ventanas de sesiones",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14))
  )

# Save the final plot
ggsave("plots/matriz_comparaciones_sesiones_4.png", grilla_final, width = 20, height = 16, units = "in", dpi = 300)
