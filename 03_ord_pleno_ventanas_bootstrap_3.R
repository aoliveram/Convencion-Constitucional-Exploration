library(readr)
library(data.table)
library(dplyr)

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
  Votante = rep(votantes, each = 200),  # ID del votante repetido 200 veces
  "01-15" = reescalar(ordenamiento_1D_WNOM_al_14ago2021_bootstrap$.),
  "22-37" = reescalar(ordenamiento_1D_WNOM_22_37_bootstrap$.) 
)

# Realizar un test t para cada votante
orden_votantes_t <- ordenamientos_14ago2021_22_37 %>%
  group_by(Votante) %>%
  summarize(
    p_valor = t.test("01-15", "22-37", paired = TRUE)$p.value
  )

print(orden_votantes_t, n=41)

# Hacemos un gráfico de barras para ilustrar los valores-p
ggplot(orden_votantes_t, aes(x = Votante, y= p_valor,fill= -p_valor) ) +          
  geom_bar(width = 0.9, stat="identity", position = position_dodge())+ 
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size=1.5)+
  labs(y = "p-value") +
  ggtitle("Contraste de hipótesis")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.y = element_text(size = 15))+
  scale_x_continuous(breaks = 1:41)

# No se puede decir con estos datos que la ideología difiere según método.

#------------------- Comparación valores de estimación

Estimaciones_finales <- data.frame(
  Estimacion_Bayesiana = reescalar(result_datos_MCMC$xbar[, 1]),
  W_NOMINATE = reescalar(result_datos_wn$legislators$coord1D)
) _%>% mutate(identificadores = identificador, individuo = as.character(1:41))

Estimaciones_finales$index <- c(1:41)

# Graficamos las estimaciones de cada método
ggplot(Estimaciones_finales, aes(x = Estimacion_Bayesiana, y = W_NOMINATE, color = Estimacion_Bayesiana)) +
  geom_point() +                        # Agrega puntos
  labs(x = "Bayesiano", y = "W-NOMINATE") +  # Etiquetas de ejes
  geom_point()+
  geom_text(aes(label = individuo), vjust = -1)+
  ggtitle("Estimaciones")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.y = element_text(size = 15))+
  scale_color_gradient(low = "red", high = "blue")

# Calculamos correlación
cor(reescalar(result_datos_MCMC$xbar[,1]), reescalar(result_datos_wn$legislators$coord1D))
