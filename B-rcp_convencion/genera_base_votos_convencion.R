# carga el listado de convencionales constituyentes
l <- list.files("rcp_convencion/votos_convencion/")
listado <- paste0("rcp_convencion/votos_convencion/", l[1])
listado <- read.csv(listado, header = F, sep="\n", encoding = "UTF-8")
listado <- as.data.frame(listado)

nombres <- listado[-c(1:3),]
nombres <- as.data.frame(nombres)

for(k in 1:NROW(nombres)){
  nombres$nombres[k] <- ifelse(nombres$nombres[k]=="Vidal, Rossana",'Vidal, Loreto',nombres$nombres[k])
  nombres$nombres[k] <- ifelse(nombres$nombres[k]=="Martín, Juan José",'Martin, Juan José',nombres$nombres[k])
  nombres$nombres[k] <- ifelse(nombres$nombres[k]=="González, Dayana",'González, Dayyana',nombres$nombres[k])
}

afavor <- NA
encontra <- NA
abstencion <- NA
dispensado <- NA

for(i in 2:length(l)){
  # carga los votos
  archivo <- paste0("rcp_convencion/votos_convencion/", l[i])
  v <- read.csv(archivo, header = F, sep="\n", encoding = "UTF-8")
  
  for(k in 1:NROW(v)){
    v$V1[k] <- ifelse(v$V1[k]=="Vidal, Rossana",'Vidal, Loreto',v$V1[k])
    v$V1[k] <- ifelse(v$V1[k]=="Martín, Juan José",'Martin, Juan José',v$V1[k])
    v$V1[k] <- ifelse(v$V1[k]=="González, Dayana",'González, Dayyana',v$V1[k])
  }
  
  a1 <- v[1,]
  a2 <- v[2,]
  a3 <- v[3,]
  
  c <- c("A Favor", "En Contra", "Abstención", "Dispensado")
  z <- 4
  b1 <- NA
  b2 <- NA
  b3 <- NA
  b4 <- NA
  
  for(z in 4:NROW(v)){
    if(grepl(c[1],v[z,])==T){b1 <- z}
    if(grepl(c[2],v[z,])==T){b2 <- z}
    if(grepl(c[3],v[z,])==T){b3 <- z}
    if(grepl(c[4],v[z,])==T){b4 <- z}
  }
  
  if((is.na(b1)==F & is.na(b2)==F)==T){afavor <- v[b1+1:b2-1,]}
  if((is.na(b2)==F & is.na(b3)==F)==T){encontra <- v[b2+1:b3-1,]}
  if((is.na(b3)==F & is.na(b4)==T)==T){abstencion <- v[b3+1:NROW(v),]}
  if((is.na(b3)==F & is.na(b4)==F)==T){abstencion <- v[b3+1:b4-1,]}
  if((is.na(b4)==F)==T){dispensado <- v[b4+1:NROW(v),]}
  
  nombre_columna <- gsub(".csv","",l[i])
  j <- NCOL(listado)
  g <- matrix(NA,nrow=155,ncol=1)
  g <- as.data.frame(g)
  colnames(g)[1] <- nombre_columna

  for(q in 1:NROW(nombres)){
    if(nombres$nombres[q] %in% afavor){g[q,] <- "A favor"}
    if(nombres$nombres[q] %in% encontra){g[q,] <- "En contra"}
    if(nombres$nombres[q] %in% abstencion){g[q,] <- "Abstencion"}
    if(nombres$nombres[q] %in% dispensado){g[q,] <- "Dispensado"}
    
  }
  
  agregando_votos <- rbind(a1,a2,a3,g)
  listado <- cbind(listado,agregando_votos)
  
}

for(k in 1:NROW(listado)){
  listado$V1[k] <- ifelse(listado$V1[k]=="Vidal, Rossana",'Vidal, Loreto',listado$V1[k])
  listado$V1[k] <- ifelse(listado$V1[k]=="Martín, Juan José",'Martin, Juan José',listado$V1[k])
  listado$V1[k] <- ifelse(listado$V1[k]=="González, Dayana",'González, Dayyana',listado$V1[k])
}


v <- date()
v1 <- substr(v, start = 5, stop = 7)
v2 <- substr(v, start = 9, stop = 10)
write.csv(listado, paste0("rcp_convencion/votos_para_rollcall",v1,"_",v2,".csv"))

