# Votaciones en el pleno hasta el 14 de Agosto de 2021. 147 Votaciones
votaciones_al_14ago2021 <- read_csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv", locale = locale(encoding = "LATIN1"))
#votaciones_al_14ago2021 <- read.csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv")
votaciones_al_14ago2021 <- votaciones_al_14ago2021[,-1] # Quito la primera columna

# Guardo los votantes y quito columna
votantes <- as.vector(votaciones_al_14ago2021[[1]])
votaciones_al_14ago2021 <- votaciones_al_14ago2021[,-1]

#------------------------------------------------------------------------------
# Estimación de Ordenamiento Político usando W-Nominate 
#------------------------------------------------------------------------------

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
ordenamiento_wnom <- wnominate(
  votaciones_al_14ago2021_rc, 
  dims = 2, 
  polarity = c(87,87) # anclamos en marinovic
)

# Gráfico de los resultados
plot(ordenamiento_wnom)

# Creamos data frame con los resultados de la dimensión 1.
ordenamiento_1D_wnom <- data.frame(posicion_ideologica = ordenamiento_wnom$legislators$coord1D)

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

ordenamiento_1D_wnom <- reescalar(ordenamiento_1D_wnom) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:155)) %>%
  arrange(posicion_ideologica)

# Grabamos los índices de cada candidato (equivalente a las posiciones de izquierda a derecha)
ordenamiento_1D_wnom$posicion_izq_der <- row.names(ordenamiento_1D_wnom)

# Graficamos de mayor a menor las posiciones de los votantes
library(ggplot2)
ggplot(ordenamiento_1D_wnom, aes(x = as.numeric(posicion_izq_der), y = posicion_ideologica, color = posicion_ideologica)) +
  geom_point() +
  geom_text(aes(label = n_votante), vjust = -1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  ggtitle("W-NOMINATE")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "Posición ideológica")+
  theme(axis.title.y = element_text(size = 15))+
  scale_color_gradient(low = "red", high = "blue")

ggplot(ordenamiento_1D_wnom, aes(x = as.numeric(posicion_ideologica), y = reorder(nombre_votante, posicion_ideologica), color = 'grey')) +
  geom_point() +
  labs(
    x = "Posición ideológica estimada W-Nominate",
    y = "Convencional",
    color = "No listas"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

#------------------------------------------------------------------------------
# Estimación de Ordenamiento Político usando MCMC Bayesiano
#------------------------------------------------------------------------------

# Ejecutar el modelo MCMC
ordenamiento_MCMC <- ideal(
  votaciones_al_14ago2021_rc,
  codes = votaciones_al_14ago2021_rc$codes,
  maxiter = 8000,  # Número máximo de iteraciones
  burnin = 1000,   # Número de iteraciones para el periodo de "quemado"
  thin = 40,      # Intervalo para guardar muestras
  normalize = T   # Normalizar los datos (ver documentación para más detalles)
)

# Rescatamos las posiciones de los candidatos y las convertimos en df.
ordenamiento_1D_MCMC <- ordenamiento_MCMC$xbar %>% 
  as.data.frame() %>%
  rename(posicion_ideologica = D1)
ordenamiento_1D_MCMC$posicion_ideologica <- -ordenamiento_1D_MCMC$posicion_ideologica

# Reescalamos los valores
ordenamiento_1D_MCMC <- reescalar(ordenamiento_1D_MCMC) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:155)) %>%
  arrange(posicion_ideologica)

# Grabamos los índices de cada candidato
rownames(ordenamiento_1D_MCMC) <- NULL
ordenamiento_1D_MCMC$posicion_izq_der <- c(1:155)

print(ordenamiento_1D_MCMC)

# Graficamos de mayor a menor las posiciones de los votantes
ggplot(ordenamiento_1D_MCMC, aes(x = as.numeric(posicion_izq_der), y = posicion_ideologica, color = posicion_ideologica)) +
  geom_point() +
  geom_text(aes(label = n_votante), vjust = -1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  ggtitle("Bayesiano")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "Posición ideológica")+
  theme(axis.title.y = element_text(size = 15))+
  scale_color_gradient(low = "red", high = "blue")

ggplot(ordenamiento_1D_MCMC, aes(x = as.numeric(posicion_ideologica), y = reorder(nombre_votante, posicion_ideologica), color = 'grey')) +
  geom_point() +
  labs(
    x = "Posición ideológica estimada MCMC",
    y = "Convencional",
    color = "No listas"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))


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
