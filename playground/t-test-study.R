# Crear función
realizar_t_test <- function(df, col1, col2) {
  t_result <- t.test(df[[col1]], df[[col2]], paired = TRUE)
  data.frame(
    comparacion = paste(col1, "vs", col2),
    p_valor = t_result$p.value,
    diferencia_media = t_result$estimate
  )
}

# Crear datos de prueba
set.seed(123)  # Para reproducibilidad

n <- 500  # Número de observaciones

df_prueba <- data.frame(
  antes = rnorm(n, mean = 50, sd = 10),
  despues = rnorm(n, mean = 55, sd = 10)  # Media ligeramente mayor
)

# Ver primeros datos
head(df_prueba)

# Aplicar la función
resultado <- realizar_t_test(df_prueba, "antes", "despues")
print(resultado)

# -----------------------------------------------------------------------------

# Cargar tidyverse para funciones útiles
library(tidyverse)

# Leer el archivo
df1 <- read_csv("data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_01-15_bootstrap.csv")
df2 <- read_csv("data - pleno/ordenamientos_pleno/ordenamiento_1D_WNOM_76-99_bootstrap.csv")

# Filtrar las filas para la persona específica
df1_filtrado <- df1 %>%
  filter(legislador == "Abarca Gonzalez, Damaris")

df2_filtrado <- df2 %>%
  filter(legislador == "Abarca Gonzalez, Damaris")

# Calcular la media de la columna cood1D
media_cood1D_01_15 <- mean(df1_filtrado$coord1D, na.rm = TRUE)
media_cood1D_76_99 <- mean(df2_filtrado$coord1D, na.rm = TRUE)

media_cood1D_01_15 - media_cood1D_76_99
