#library(readr)
#votaciones_al_14ago2021 <- read_csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv", locale = locale(encoding = "LATIN1"))
#votaciones_al_14ago2021_E <- read.csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv")
#votaciones_al_14ago2021 <- votaciones_al_14ago2021[,-1] # Quito la primera columna

# Guardo los votantes y quito columna
#votantes <- as.vector(votaciones_al_14ago2021[[1]])
#votaciones_al_14ago2021 <- votaciones_al_14ago2021[,-1]

library(readr)
library(wnominate)

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
# Estimación de Ordenamiento Político usando MCMC Bayesiano
#------------------------------------------------------------------------------

votaciones_al_14ago2021_manual_2 <-read.csv("Pleno/votaciones_al_14ago2021_manual_2.csv") # __ votaciones

votantes <- as.vector(votaciones_al_14ago2021_manual_2[[1]])
votaciones_al_14ago2021_manual_2 <- votaciones_al_14ago2021_manual_2[,-1]

votaciones_al_14ago2021_rc <- rollcall(
  votaciones_al_14ago2021_manual_2,             
  yea = c(1),
  nay = c(0),
  missing = c(NA),
  notInLegis = NULL,
  legis.names = votantes,
  desc = "Votaciones Convención Constitucional al 14 Ago 2021"
)

ordenamiento_MCMC <- ideal(
  votaciones_al_14ago2021_rc,
  codes = votaciones_al_14ago2021_rc$codes,
  maxiter = 8000,  # Número máximo de iteraciones
  burnin = 1000,   # Número de iteraciones para el periodo de "quemado"
  thin = 40,      # Intervalo para guardar muestras
  normalize = T   # Normalizar los datos (ver documentación para más detalles)
)

ordenamiento_1D_MCMC <- ordenamiento_MCMC$xbar %>% 
  as.data.frame() %>%
  rename(posicion_ideologica = D1)
ordenamiento_1D_MCMC$posicion_ideologica <- -ordenamiento_1D_MCMC$posicion_ideologica

ordenamiento_1D_MCMC <- reescalar(ordenamiento_1D_MCMC) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:155)) %>%
  arrange(posicion_ideologica)

rownames(ordenamiento_1D_MCMC) <- NULL
ordenamiento_1D_MCMC$posicion_izq_der <- c(1:155)

print(ordenamiento_1D_MCMC)

write.csv(ordenamiento_1D_MCMC, 
          file = "Pleno/ordenamientos_pleno/ordenamiento_1D_MCMC_14-07_14-08.csv", 
          row.names = FALSE)




















# ------------------------ Plots posiciones
library(ggplot2)
ggplot(ordenamiento_1D_MCMC, aes(x = posicion_ideologica, y = reorder(nombre_votante, posicion_ideologica), color = 'grey')) +
  geom_point() +
  labs(
    x = "Posición ideológica estimada MCMC",
    y = "Convencional",
    color = "No listas"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))
