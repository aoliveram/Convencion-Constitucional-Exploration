################################################################################
# funciones 
################################################################################

# Crea una base dicotómica a partir de inputs

dicotomiza <- function(datos, id,label){ # label para la columna
  z <- NA
  x <- as.numeric(matrix(NA,nrow=NROW(datos), ncol=1))
  for(i in 1:NCOL(datos)){ # para cada pregunta
    alternativas <- unique(datos[,i])
    alternativas.num <- length(unique(datos[,i]))
    for(j in 1:alternativas.num){ # para cada alternativa de cada pregunta
      z.aux <- paste(label,i,"_",j,sep="")
      z <- cbind(z,z.aux)
      x.aux <- matrix(NA,nrow=NROW(datos), ncol=1)
      for(k in 1:NROW(datos)){ # para cada candidato
        x.aux[k] <- ifelse((datos[k,i]==alternativas[j])==T,1,0)
      }
      x <- cbind(x,x.aux)
    }
  }
  
  x <- as.data.frame(x[,2:NCOL(x)])
  
  votos <- cbind(id,x)
  colnames(votos) <- z
  colnames(votos)[1] <- "candidato"
  return(votos)
}

# calcula wd-nominate 

ideol_estimada_nominate <- function(x,id,polar){
  candidatos <- id
  rc_total <- rollcall(x,             
                       yea=c(1), # reduce los valores a dos grupos yea/nay
                       nay=c(0),
                       missing=NA,
                       notInLegis=NULL, # vector de ausentes en que seccion
                       legis.names=candidatos,
                       legis.data=NULL,
                       desc="Todo Chile")
  
  # polar <- "cc87" # Tere Marinovic
  # z <- x[x$id %in% polar,]
  # z0 <- ifelse(NROW(z)!=0,z,2)
  
  result_total <- wnominate(rc_total, dims=2, polarity=c(polar,polar)) # Tere Marinovic
  return(result_total)
}