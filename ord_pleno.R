# Votaciones en el pleno hasta el 14 de Agosto de 2021. 147 Votaciones
library(readr)
votaciones_al_14ago2021 <- read_csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv", locale = locale(encoding = "LATIN1"))
#votaciones_al_14ago2021_E <- read.csv("rcp_convencion/RCP_votaciones_al_14ago2021.csv")
votaciones_al_14ago2021 <- votaciones_al_14ago2021[,-1] # Quito la primera columna

# Guardo los votantes y quito columna
votantes <- as.vector(votaciones_al_14ago2021[[1]])
votaciones_al_14ago2021 <- votaciones_al_14ago2021[,-1]

#------------------------------------------------------------------------------
# Estimación de Ordenamiento Político usando W-Nominate 
#------------------------------------------------------------------------------

# Crear un objeto de clase rollcall para el análisis con wnominate
# install.packages('wnominate')
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
ordenamiento_WNOM <- wnominate(
  votaciones_al_14ago2021_rc, 
  dims = 2, 
  trials=1,
  polarity = c(87,87) # anclamos en marinovic
)

# Gráfico de los resultados
plot(ordenamiento_WNOM)

# Creamos data frame con los resultados de la dimensión 1.
ordenamiento_1D_WNOM <- data.frame(posicion_ideologica = ordenamiento_WNOM$legislators$coord1D)

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

ordenamiento_1D_WNOM <- reescalar(ordenamiento_1D_WNOM) %>%
  mutate(nombre_votante = votantes, n_votante = as.character(1:155)) %>%
  arrange(posicion_ideologica)

# Grabamos los índices de cada candidato (equivalente a las posiciones de izquierda a derecha)
ordenamiento_1D_WNOM$posicion_izq_der <- as.integer(row.names(ordenamiento_1D_WNOM))

# Guardamos datos
write.csv(ordenamiento_1D_WNOM, 
          file = "ordenamiento_1D_WNOM.csv", 
          row.names = FALSE)

# ------------------------ Plots posiciones

# Graficamos de mayor a menor las posiciones de los votantes
library(ggplot2)
ggplot(ordenamiento_1D_WNOM, aes(x = as.numeric(posicion_izq_der), y = posicion_ideologica, color = posicion_ideologica)) +
  geom_point() +
  geom_text(aes(label = n_votante), vjust = -1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  ggtitle("W-NOMINATE")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "Posición ideológica")+
  theme(axis.title.y = element_text(size = 15))+
  scale_color_gradient(low = "red", high = "blue")

ggplot(ordenamiento_1D_WNOM, aes(x = as.numeric(posicion_ideologica), y = reorder(nombre_votante, posicion_ideologica), color = 'grey')) +
  geom_point() +
  labs(
    x = "Posición ideológica estimada W-Nominate",
    y = "Convencional",
    color = "No listas"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))


# --------------------- Bootstrap manual para estimar la incertidumbre en WNOMINATE

muestra_votos <- function(base_datos, N) {
  # Seleccionamos N votaciones con reemplazo
  votos_muestreados <- sample(names(base_datos), N, replace = TRUE)
  # En lugar de usar select(), extraemos las columnas manualmente para permitir duplicados
  muestras_votos <- base_datos[, votos_muestreados, drop = FALSE]
  
  return(muestras_votos)
}

# Crear un dataframe para almacenar las estimaciones
ordenamiento_1D_boostraping_WNOM <- data.frame()

# Realizar el bootstraping
n_iter <- 200

for (i in 1:n_iter) {
  if (i==1) start_time <- Sys.time()
  
  cat('Num iter:', i, '/', n_iter, "\n")
  
  # Generar una muestra aleatoria
  muestras_aleatorias <- muestra_votos(votaciones_al_14ago2021, 100)
  
  # Crear un objeto de clase rollcall para el análisis con wnominate
  votaciones_al_14ago2021_bootstrap_rc <- rollcall(
    muestras_aleatorias,             
    yea = c(1),
    nay = c(0),
    missing = c(NA),
    notInLegis = NULL,
    legis.names = votantes,
    desc = "100 Votaciones Convención Constitucional al 14 Ago 2021"
  )
  
  # Calcular la ideología con wnominate (ajusta esto según tus datos y necesidades)
  ordenamiento_submuestra_WNOM <- wnominate(votaciones_al_14ago2021_bootstrap_rc, dims=2, polarity=c(87,87))
  
  # Agregar la estimación al dataframe de resultados
  ordenamiento_1D_boostraping_WNOM <- rbind(
    ordenamiento_1D_boostraping_WNOM,
    data.frame(iteracion = i, posicion_ideologica = ordenamiento_submuestra_WNOM$legislators$coord1D)
  )
  
  if (i==n_iter) end_time <- Sys.time()
}

execution_time <- end_time - start_time
execution_time # 5.78 mins

write.csv(ordenamiento_1D_boostraping_WNOM, 
          file = "ordenamiento_1D_boostraping_WNOM.csv", 
          row.names = FALSE)


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

# Guardamos datos
write.csv(ordenamiento_1D_MCMC, 
          file = "ordenamiento_1D_MCMC.csv", 
          row.names = FALSE)

# ------------------------ Plots posiciones

# Graficamos de mayor a menor las posiciones de los votantes
ggplot(ordenamiento_1D_MCMC, aes(x = posicion_izq_der, y = posicion_ideologica, color = posicion_ideologica)) +
  geom_point() +
  geom_text(aes(label = n_votante), vjust = -1) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  ggtitle("Bayesiano")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y = "Posición ideológica")+
  theme(axis.title.y = element_text(size = 15))+
  scale_color_gradient(low = "red", high = "blue")

ggplot(ordenamiento_1D_MCMC, aes(x = posicion_ideologica, y = reorder(nombre_votante, posicion_ideologica), color = 'grey')) +
  geom_point() +
  labs(
    x = "Posición ideológica estimada MCMC",
    y = "Convencional",
    color = "No listas"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))


# --------------------- Bootstrap manual para estimar la incertidumbre en IDEAL

# Crear un dataframe para almacenar las estimaciones
ordenamiento_1D_boostraping_MCMC <- data.frame()

# Realizar el bootstraping
n_iter <- 200

for (i in 1:n_iter) {
  if (i==1) start_time <- Sys.time()
  
  cat('Num iter:', i, '/', n_iter, "\n")
  
  # Generar una muestra aleatoria
  muestras_aleatorias <- muestra_votos(votaciones_al_14ago2021, 100)
  
  # Crear un objeto de clase rollcall para el análisis con IDEAL
  votaciones_al_14ago2021_bootstrap_rc <- rollcall(
    muestras_aleatorias,             
    yea = c(1),
    nay = c(0),
    missing = c(NA),
    notInLegis = NULL,
    legis.names = votantes,
    legis.data=NULL,
    desc = "100 Votaciones Convención Constitucional al 14 Ago 2021"
  )
  
  # Calcular la ideología con wnominate (ajusta esto según tus datos y necesidades)
  ordenamiento_submuestra_MCMC <- ideal(
    votaciones_al_14ago2021_bootstrap_rc,
    codes = votaciones_al_14ago2021_bootstrap_rc$codes,
    maxiter = 8000,  # Número máximo de iteraciones
    burnin = 1000,   # Número de iteraciones para el periodo de "quemado"
    thin = 40,      # Intervalo para guardar muestras
    normalize = T   # Normalizar los datos (ver documentación para más detalles)
  )
  
  # Agregar la estimación al dataframe de resultados
  ordenamiento_1D_boostraping_MCMC <- rbind(
    ordenamiento_1D_boostraping_MCMC,
    data.frame(iteracion = i, posicion_ideologica = as.numeric(ordenamiento_submuestra_MCMC$xbar[,1]))
  )
  
  if (i==n_iter) end_time <- Sys.time()
}

execution_time <- end_time - start_time
execution_time # 22.93 mins

write.csv(ordenamiento_1D_boostraping_MCMC, 
          file = "ordenamiento_1D_boostraping_MCMC.csv", 
          row.names = FALSE)

# Extraer los valores de ideología de cada votante
valores_ideologia_MCMC <- ordenamiento_submuestra_MCMC[, 2] %>% data.frame()


#------------------------------------------------------------------------------
# Comparación con estimaciones de https://github.com/jfabregalacoa/rcp_convencion
#------------------------------------------------------------------------------

# Cargamos datos de Jorge
library(readr)
ordenamiento_rcp <- as.data.frame(read_csv("rcp_convencion/RCP_estimacion_ideologia.csv", locale = locale(encoding = "LATIN1")))
ordenamiento_1D_rcp <- ordenamiento_rcp[ , c('nombres', 'ideologia', 'lista')]
ordenamiento_1D_rcp$posicion_izq_der <- c(155:1)

ordenamiento_1D_rcp <- ordenamiento_1D_rcp %>%
  rename(
    posicion_ideologica = ideologia,
    nombre_votante = nombres
  ) %>%
  left_join(ordenamiento_1D_WNOM %>% select(nombre_votante, n_votante), by = "nombre_votante") %>%
  select(posicion_ideologica, nombre_votante, n_votante, posicion_izq_der, lista)

str(ordenamiento_1D_rcp)

write.csv(ordenamiento_1D_rcp, 
          file = "rcp_convencion/ordenamiento_1D_rcp.csv", 
          row.names = FALSE)

# ------------------------- Comparamos

ordenamiento_1D_MCMC <- read.csv("ordenamiento_1D_MCMC.csv")
ordenamiento_1D_WNOM <- read.csv("ordenamiento_1D_WNOM.csv")
ordenamiento_1D_rcp <- read.csv("rcp_convencion/ordenamiento_1D_rcp.csv")
ordenamiento_1D <- merge(ordenamiento_1D_MCMC, ordenamiento_1D_WNOM, by = "nombre_votante", suffixes = c("_MCMC", "_WNOM"))
colnames(ordenamiento_1D_rcp)[colnames(ordenamiento_1D_rcp) == "posicion_ideologica"] <- "posicion_ideologica_RCP"
ordenamiento_1D <- merge(ordenamiento_1D, ordenamiento_1D_rcp, by = "nombre_votante")

library(ggplot2)
ggplot(ordenamiento_1D) +
  geom_point(aes(x = posicion_ideologica_RCP, y = reorder(nombre_votante, posicion_ideologica_RCP), color = "RCP"), size = 3) +
  geom_point(aes(x = posicion_ideologica_MCMC, y = reorder(nombre_votante, posicion_ideologica_MCMC), color = "MCMC"), size = 3) +
  geom_point(aes(x = posicion_ideologica_WNOM, y = reorder(nombre_votante, posicion_ideologica_WNOM), color = "WNOM"), size = 3) +
  scale_color_manual(values = c("MCMC" = "blue", "WNOM"="green", "RCP" = "red"), name = "Fuente") +
  labs(
    title = "Comparación de Ordenamiento Ideológico",
    x = "Posición Ideológica",
    y = "Nombre del Votante"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "right"
  )


# ------------------- ¿Son iguales los nombres de RCP y los de votaciones_al_14ago2021?

library(dplyr)

# Cargamos los archivos CSV
ordenamiento_1D_boostraping_WNOM_parallel <- read.csv("ordenamiento_1D_boostraping_WNOM_parallel.csv")
ordenamiento_1D_rcp <- read.csv("rcp_convencion/ordenamiento_1D_rcp.csv")

# Veamos si usan los mismos nombres
ordenamiento_1D_boostraping_WNOM_parallel$legislador
ordenamiento_1D_rcp$nombres

nombres_coinciden_dir1 <- all(ordenamiento_1D_boostraping_WNOM_parallel$legislador %in% ordenamiento_1D_rcp$nombres)
print(nombres_coinciden_dir1)
nombres_coinciden_dir2 <- all(ordenamiento_1D_rcp$nombres %in% ordenamiento_1D_boostraping_WNOM_parallel$legislador)
print(nombres_coinciden_dir2)

# En efecto, no hay nombres en uno que no estén en el otro. Sí sin iguales

