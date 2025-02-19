#------------------------------------------------------------------------------
# Estimación de Ordenamiento Político usando W-Nominate 
#------------------------------------------------------------------------------

# Votaciones en el pleno hasta el 14 de Agosto de 2021. 147 Votaciones
votaciones_al_14ago2021 <- read_csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv", locale = locale(encoding = "LATIN1"))
#votaciones_al_14ago2021 <- read.csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv")
votaciones_al_14ago2021 <- votaciones_al_14ago2021[,-1] # Quito la primera columna

# Guardo los votantes y quito columna
votantes <- as.vector(votaciones_al_14ago2021[[1]])
votaciones_al_14ago2021 <- votaciones_al_14ago2021[,-1]

# Crear un objeto de clase rollcall para el análisis con wnominate
install.packages('wnominate')
library(wnominate)

votaciones_al_14ago2021_rc <- rollcall(
  votaciones_al_14ago2021,             
  yea = c(1),
  nay = c(0),
  missing = c(NA),
  notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Convención Constitucional al 14 Ago 2021"
)

# Ejecutar el análisis W-Nominate
ordenamiento_al_14ago2021 <- wnominate(
  votaciones_al_14ago2021_rc, 
  dims = 2, 
  polarity = c(87,87) # anclamos en marinovic
)

# Gráfico de los resultados
plot(ordenamiento_al_14ago2021)

# Creamos data frame con los resultados de la dimensión 1.
ordenamiento_al_14ago2021_1D <- data.frame(posicion_ideologica = ordenamiento_al_14ago2021$legislators$coord1D)

# Reescalamos dentro de [-1,1]
reescalar <- function(vector_original) {
  min_original <- min(vector_original)
  max_original <- max(vector_original)
  a <- -1
  b <- 1
  vector_reescalado <- ((vector_original - min_original) / (max_original - min_original)) * (b - a) + a
  return(vector_reescalado)
}

library(dplyr)

ordenamiento_al_14ago2021_1D <- reescalar(ordenamiento_al_14ago2021_1D) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:155)) %>%
  arrange(posicion_ideologica)

# Grabamos los índices de cada candidato (equivalente a las posiciones de izquierda a derecha)
ordenamiento_al_14ago2021_1D$posicion_izq_der <- row.names(ordenamiento_al_14ago2021_1D)

# Graficamos de mayor a menor las posiciones de los votantes
library(ggplot2)
ggplot(ordenamiento_al_14ago2021_1D, aes(x = as.numeric(posicion_izq_der), y = posicion_ideologica, color = posicion_ideologica)) +
  geom_point() +
  geom_text(aes(label = n_votante), vjust = -1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  ggtitle("W-NOMINATE")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "Posición ideológica")+
  theme(axis.title.y = element_text(size = 15))+
  scale_color_gradient(low = "red", high = "blue")

ggplot(ordenamiento_al_14ago2021_1D, aes(x = as.numeric(posicion_ideologica), y = reorder(nombre_votante, posicion_ideologica), color = 'grey')) +
  geom_point() +
  labs(
    x = "Posición ideológica estimada",
    y = "Convencional",
    color = "No listas"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

#------------------------------------------------------------------------------
# Estimación de Ordenamiento Político usando MCMC Bayesiano
#------------------------------------------------------------------------------

# Ejecutar el modelo MCMC
result_datos_MCMC <- ideal(rc_datos,
                           codes = rc_datos$codes,
                           maxiter = 8000,  # Número máximo de iteraciones
                           burnin = 1000,   # Número de iteraciones para el periodo de "quemado"
                           thin = 40,      # Intervalo para guardar muestras
                           normalize = T)   # Normalizar los datos (ver documentación para más detalles)

# Rescatamos las posiciones de los candidatos y las convertimos en df.
Resultados_1D_MCMC <- result_datos_MCMC$xbar %>% 
  as.data.frame() %>%
  rename(posición_ideológica = D1)

# Reescalamos los valores
Resultados_1D_MCMC_reescalados <- reescalar(Resultados_1D_MCMC) %>%
  mutate(identificadores = identificador, individuo = as.character(1:41)) %>%
  arrange(posición_ideológica)

print(Resultados_1D_MCMC_reescalados)

# Grabamos los índices de cada candidato
Resultados_1D_MCMC_reescalados$index <- c(1:41)

# Graficamos de mayor a menor las posiciones de los votantes
ggplot(Resultados_1D_MCMC_reescalados, aes(x = as.numeric(index), y = posición_ideológica, color = posición_ideológica)) +
  geom_point() +
  geom_text(aes(label = individuo), vjust = -1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  ggtitle("Bayesiano")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "Posición ideológica")+
  theme(axis.title.y = element_text(size = 15))+
  scale_color_gradient(low = "red", high = "blue")


#------------------------------------------------------------------------------
# Comparación con https://github.com/jfabregalacoa/rcp_convencion
#------------------------------------------------------------------------------

library(readr)
ordenamiento_rcp <- read_csv("rcp_convencion/RCP_estimacion_ideologia.csv", locale = locale(encoding = "LATIN1"))

ggplot(ordenamiento_rcp, aes(x = ideologia, y = reorder(nombres, ideologia), color = lista)) +
  geom_point() +
  #geom_errorbarh(aes(xmin = ideologia - sd1, xmax = ideologia + sd1), height = 0.2) +
  labs(
    x = "Posición ideológica estimada",
    y = "Convencional",
    color = "Lista"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

cor(as.numeric(ordenamiento_al_14ago2021_1D$posicion_izq_der), ordenamiento_rcp$ideologia)
