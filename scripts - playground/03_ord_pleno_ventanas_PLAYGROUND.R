library(readr)
library(doParallel)
library(data.table)
library(pscl)
library(dplyr)

normalizar_nombres <- function(texto) {
  texto_limpio <- iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT")
  return(texto_limpio)
}

# Crear votantes_norm ANTES del cluster
votaciones_16_21 <- read.csv("Pleno/votaciones_16_21.csv")
votantes <- as.vector(votaciones_16_21[[1]])
votantes_norm <- normalizar_nombres(votantes)
votaciones_16_21 <- votaciones_16_21[,-1]

# Configurar cluster SIN clusterExport
cl <- makeCluster(12, type="FORK", clean=TRUE)
registerDoParallel(cl)

parallel_bootstrap_ideal <- function(votaciones, votantes_norm, N_votos, 
                                     n_iter=200, constrain_list=NULL,  maxiter = 8000, burnin = 1000, thin = 40) {
  
  foreach(i=1:n_iter, .combine="rbind", .packages=c("pscl","data.table")) %dopar% {
    
    set.seed( (as.numeric(Sys.time()) * Sys.getpid()) %% .Machine$integer.max )
    
    tryCatch({
      muestras <- votaciones[, sample(names(votaciones), N_votos, replace=TRUE)]
      
      rc <- rollcall(muestras,
                     yea=1, nay=0, missing=NA,
                     legis.names=votantes_norm,
                     vote.names=colnames(muestras))
      
      # Identificación del modelo
      if(is.null(constrain_list)){
        constrain_list <- list(
          sample(votantes_norm,1) = c(-2),
          sample(votantes_norm,1) = c(2)
        )
      }
      priors <- pscl::constrain.legis(rc, x=constrain_list, d=1)
      
      ideal_fit <- pscl::ideal(rc,
                               priors=priors$priors,
                               startvals=priors$startvals,
                               burnin = burnin,
                               maxiter = maxiter,
                               thin = thin,
                               normalize = TRUE,
                               verbose = FALSE)
      
      data.table(
        iteracion = i,
        legislador = rownames(ideal_fit$xbar),
        coord1D = ideal_fit$xbar[,1],
        se1D = apply(ideal_fit$x[,,1], 1, sd)
      )
      
    }, error=function(e) data.table())
  }
}

# Ejecutar con restricciones explícitas
resultados <- parallel_bootstrap_ideal(
  votaciones_16_21,
  votantes_norm,
  N_votos = round(ncol(votaciones_16_21)*0.7),
  constrain_list = list("LEGISLADOR1"=-2, "LEGISLADOR2"=2), # Ejemplo real
  maxiter=8000,
  burnin=1000,
  thin=40
)

stopCluster(cl)
