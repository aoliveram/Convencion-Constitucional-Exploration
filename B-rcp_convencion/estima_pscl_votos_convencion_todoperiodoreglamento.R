# estima ideología en base a pscl para votaciones en CC
# https://www.nature.com/articles/s41598-020-74175-w para redes
# https://github.com/schochastics/congress
# https://cran.r-project.org/web/packages/backbone/vignettes/backbone.html 
  
  rm(list=ls())
  require(here)
  library(pscl)
  aqui <- here()
  source(paste0(aqui,"/code/funciones.R"))
  
  # votos de presidente y vicepresidente
  presidente <- read.csv(paste0(aqui,"/data/votos_presidente_vicepresidente.csv"),
                         sep=";")
  presidente_id <- presidente[-c(1,2),4]
  colnames(presidente)[3] <- "nombres"
  voto_presidente <- presidente[-c(1,2),-c(1,2)]
  id <- voto_presidente[,c(1)]
  voto_presidente <- voto_presidente[,-c(1:3)]
  voto_presidente_dicotomizado <- dicotomiza(voto_presidente,id,"pre_vice_")
  
  # votos a favor/en contra, etc.
  
  roll <- list.files(paste0(aqui,"/data/"),pattern = "votos_para_rollcall")
  
  archivo_votos_usado <- "votos_para_rollcallApr_10.csv"
  base_votos <- read.csv(paste0(aqui,"/data/",archivo_votos_usado))
  votos <- base_votos[4:NROW(base_votos),3:NCOL(base_votos)]
  candidato <- base_votos[4:NROW(base_votos),2]
  
  for(i in 1:NCOL(votos)){
    votos[,i] <- gsub("A favor",1,votos[,i])
    votos[,i] <- gsub("En contra",0,votos[,i])
    votos[,i] <- gsub("Abstencion",NA,votos[,i])
    votos[,i] <- gsub("Dispensado",NA,votos[,i])
  }
  
  votos <- cbind(candidato,votos)
  
  ### junta los votos "A favor"... con los votos dicotomizados
  votos_z <- merge(voto_presidente_dicotomizado,votos) 
  
  ## se guarda los nombres de los convencionales
  candidato <- votos[,c(1)]
  
  ## se guarda base con sólo los votos como resumen
  write.csv(votos,paste0(aqui,"/data/resumen_",archivo_votos_usado))
  votos <- votos[,-c(1)]
  
  ### estimación de posiciones ideologicas
  
  rc_total <- rollcall(votos,             
                       yea=c(1), # reduce los valores a dos grupos yea/nay
                       nay=c(0),
                       notInLegis=NULL, # vector de ausentes en que seccion
                       legis.names=candidato,
                       legis.data=NULL,
                       desc="Convención")

require(wnominate)
result_total_nominate <- wnominate(rc_total, dims=2, polarity=c(26,26))
summary(result_total_nominate)

# una dimension
mcmc_e1 <- pscl::ideal(rc_total, 
                      codes = rc_total$codes,
                      maxiter = 10000, 
                      burnin = 1000, 
                      thin = 250,
                      d = 1, # dimensiones
                      normalize = T,
                      store.item=TRUE) 
## prediccion
prediccion1 <- predict(mcmc_e1)
summary(prediccion1)
plot(prediccion1)


# dos dimensiones
mcmc_e <- pscl::ideal(rc_total, 
                      codes = rc_total$codes,
                      maxiter = 10000, 
                      burnin = 1000, 
                      thin = 250,
                      d = 2, # dimensiones
                      normalize = T,
                      store.item=TRUE) # ver help para detalles

## prediccion
prediccion <- predict(mcmc_e)
summary(prediccion)
plot(prediccion)



# juntamos el resultado de IDEAL con la bbdd de candidatos
resumen <- summary(mcmc_e)
resumen_tabla <- cbind(resumen$xm)
resumen_tabla <- cbind(row.names(resumen_tabla),resumen_tabla)
resumen_tabla <- cbind(resumen_tabla,resumen$xsd)
resumen_tabla.x <- as.data.frame(resumen$xHDR)
resumen_tabla <- cbind(resumen_tabla,resumen_tabla.x)
colnames(resumen_tabla) <- c("nombres","media_d1","media_d2","sd1","sd2","min1","max1","min2","max2")
rango <- as.data.frame(resumen$xHDR)

v <- date()
v1 <- substr(v, start = 5, stop = 7)
v2 <- substr(v, start = 9, stop = 10)
write.csv(resumen_tabla,paste0(aqui,"/data/tabla_resumen_estimacion_ideal_",v1,"_",v2,".csv"))

# presidente_id <- presidente[-c(1,2),3:4]
# resumen_tabla <- merge(presidente_id,resumen_tabla)

atributos <- read.csv(paste0(aqui,"/data/atributos_electos.csv"),sep=";")
colnames(atributos) <- c("id","candidatura","nombres", "region","distrito","zona_ppoo",
                         "lista_x","lista","partido","voto_lista","voto_nombre",
                         "pct_lista","pct_nombre","genero")
atributos$color <- NA

for(i in 1:NROW(atributos)){
  if(atributos$lista[i]=="P"){atributos$color[i] <- "red4"}
  if(atributos$lista[i]=="PPOO"){atributos$color[i] <- "purple"}
  if(atributos$lista[i]=="AD"){atributos$color[i] <- "red"}
  if(atributos$lista[i]=="O"){atributos$color[i] <- "grey"}
  if(atributos$lista[i]=="A"){atributos$color[i] <- "gold"}
  if(atributos$lista[i]=="NN"){atributos$color[i] <- "cyan"}
  if(atributos$lista[i]=="CV"){atributos$color[i] <- "blue"}
}

for(k in 1:NROW(atributos)){
  atributos$nombres[k] <- ifelse(atributos$nombres[k]=="Vidal, Rossana",'Vidal, Loreto',atributos$nombres[k])
  atributos$nombres[k] <- ifelse(atributos$nombres[k]=="Martín, Juan José",'Martin, Juan José',atributos$nombres[k])
  atributos$nombres[k] <- ifelse(atributos$nombres[k]=="González, Dayana",'González, Dayyana',atributos$nombres[k])
}


resumen_tabla <- merge(atributos,resumen_tabla)
# dim 1
resumen_tabla$media_d1 <- as.character(resumen_tabla$media_d1)
resumen_tabla$media_d1 <- as.numeric(resumen_tabla$media_d1)
resumen_tabla$min1 <- as.character(resumen_tabla$min1)
resumen_tabla$min1 <- as.numeric(resumen_tabla$min1)
resumen_tabla$max1 <- as.character(resumen_tabla$max1)
resumen_tabla$max1 <- as.numeric(resumen_tabla$max1)
resumen_tabla$sd1 <- as.character(resumen_tabla$sd1)
resumen_tabla$sd1 <- as.numeric(resumen_tabla$sd1)
# dim 2
resumen_tabla$media_d2 <- as.character(resumen_tabla$media_d2)
resumen_tabla$media_d2 <- as.numeric(resumen_tabla$media_d2)
resumen_tabla$min2 <- as.character(resumen_tabla$min2)
resumen_tabla$min2 <- as.numeric(resumen_tabla$min2)
resumen_tabla$max2 <- as.character(resumen_tabla$max2)
resumen_tabla$max2 <- as.numeric(resumen_tabla$max2)
resumen_tabla$sd2 <- as.character(resumen_tabla$sd2)
resumen_tabla$sd2 <- as.numeric(resumen_tabla$sd2)

resumen_tabla$err_min1 <- resumen_tabla$media_d1 - resumen_tabla$sd1 
resumen_tabla$err_max1 <- resumen_tabla$media_d1 + resumen_tabla$sd1 
resumen_tabla$err_min2 <- resumen_tabla$media_d2 - resumen_tabla$sd2 
resumen_tabla$err_max2 <- resumen_tabla$media_d2 + resumen_tabla$sd2 

resumen_tabla <- resumen_tabla[order(resumen_tabla$media_d1),]


# reescalamiento en una escala [-1,1]
x0 <- min(resumen_tabla$media_d1)
x1 <- max(resumen_tabla$media_d1)
resumen_tabla$ideologia <- 2*(resumen_tabla$media_d1 - x0)/(x1-x0) -1

x00 <- min(resumen_tabla$media_d2)
x11 <- max(resumen_tabla$media_d2)
resumen_tabla$ideologia2 <- 2*(resumen_tabla$media_d2 - x00)/(x11-x00) -1

colfunc <- colorRampPalette(c("red", "blue"))
colores <- unlist(as.list(colfunc(7)))

write.csv(resumen_tabla,paste0(aqui,"/data/tabla_resumen_estimacion_pscl_",v1,"_",v2,".csv"))


#########################################################################
library(ggplot2)
library(RColorBrewer)

myColors <- c("#99000d","#f03b20","green", "grey","gold" ,"darkorange3","#4575b4")
names(myColors) <- levels(resumen_tabla$partido)
colScale <- scale_colour_manual(name = "partido",values = myColors)

qplot(
    x = ideologia,
    y = -ideologia2,
    size=I(2),
    xlab = "Dimensión 1",
    ylab = "Dimensión 2",
    col = lista, 
    fill=lista,
    data = resumen_tabla) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  ggtitle("Ideología estimada en dos dimensiones - Convención Constitucional") +
  annotate(geom="text", x=0.75, y=-0.75, label="@jorgefabrega",
           color="grey1") #+ colScale


grupos <- read.csv(paste0(aqui,"/data/membresias_sep2021.csv"),sep=";")
resumen_grupos <- merge(resumen_tabla,grupos)

myColors2_sep2021 <- c("#99000d","#f03b20","green", "gold","grey" ,"darkorange3","#4575b4","purple","pink","cyan")
names(myColors2_sep2021) <- levels(resumen_tabla$lista_sept2021)
colScale2 <- scale_colour_manual(name = "Grupo",values = myColors2_sep2021)

qplot(
  x = ideologia,
  y = -ideologia2,
  size=I(2),
  xlab = "Dimensión 1",
  ylab = "Dimensión 2",
  col = lista_sept2021, 
  #fill= lista_sept2021,
  data = resumen_grupos) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  ggtitle("Ideología estimada en dos dimensiones - Convención Constitucional") +
  #annotate(geom="text", x=0.75, y=-0.75, label="@jorgefabrega",color="grey1") + 
  colScale2

# dos dimensiones grupos a febrero 2022
resumen_tabla <- resumen_tabla[resumen_tabla$nombres!="Rojas, Rodrigo",]
resumen_grupos <- merge(resumen_tabla,grupos)
myColors3_feb2022 <- c("yellow",
                       "#f03b20",
                       "green4",
                       "khaki4",
                       "greenyellow",
                       "grey",
                       "black",
                       "purple",
                       "pink",
                       "violetred",
                       "coral",
                       "blue",
                       "lightblue")
names(myColors3_feb2022) <- levels(resumen_tabla$lista_feb2022)
colScale3 <- scale_colour_manual(name = "Grupo",values = myColors3_feb2022)

qplot(
  x = ideologia,
  y = -ideologia2,
  size=I(2),
  xlab = "Dimensión 1",
  ylab = "Dimensión 2",
  col = lista_feb2022, 
  data = resumen_grupos) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.title = element_blank()) +
  ggtitle("Ideología estimada en dos dimensiones - Convención Constitucional") +
  annotate(geom="text", x=0.3, y=-1, label="Fuente: elaborado por @jorgefabrega sobre la base de votaciones en el pleno\n Última actualización: 05-Mar'22",color="grey1") + 
  colScale3


# ubica a todos los convencionales en una línea y destaca algunos especificados
listado <- c("Woldarsky, Manuel")
seleccion <- resumen_tabla[resumen_tabla$nombres %in% listado,]
plot(resumen_tabla$ideologia,rep(0,NROW(resumen_tabla)), pch=19,
     col="grey",
     yaxt="n",
     ylab="",
     ylim = c(-0.1,0.3),
     main="Distribución ideológica de la convención \n En base a votaciones en el pleno",
     xlab="Izquierda ------------------------------------------------------------------------------------ Derecha")
points(seleccion$ideologia[1],0,col="red", pch=19) 
text(seleccion$ideologia,0.21,listado[1],cex=0.6)
text(0.5,-0.02,"Elaborado por @jorgefabrega",cex=0.8)
segments(seleccion$ideologia[1],0,seleccion$ideologia[1],0.2)

