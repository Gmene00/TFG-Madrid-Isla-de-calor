#Clustering
rm(list=ls())
library(cluster)
library(factoextra)
library(ggplot2)

#######METEOROLOGÍA
###############################################KMEANS
meteo21<-read.csv("Meteo/Diario/meteoDiario21_byDay.csv",sep=";", dec=",")
asocNombresNum<-read.csv("Meteo/estacion_distrito.csv",sep=";", dec=",")

#Puesto que hay muchas estaciones de las cuales tenemos bastantes nulos, vamos a realizar distintas
#aproximaciones dado el dataset que conseguimos del Ayuntamiento de Madrid

#1ª aproximacion: hay varias estaciones de las cuales disponemos datos de las 9 magnitudes
# 102, 103, 106, 106, 24, 59
vecEst<-c(102, 103, 106, 107, 24, 54, 56, 59)
sapply(meteo21, function(x) sum(is.na(x)))
dfFirst<-subset(meteo21,meteo21$Estacion %in% vecEst)
sapply(dfFirst, function(x) sum(is.na(x)))
#Quitamos las magnitudes 87 y 88 ya que tienen muchos NA
dfFirst<-dfFirst[,-c(10,11)]


#Ahora por estacion vamos a sacar la media de sus magnitudes.
dfMeans<-NULL#dataframe con las medias de las magnitudes de cada estacion
for(estacion in unique(dfFirst$Estacion)){
  estMean<-NULL
  a<-dfFirst[dfFirst$Estacion==estacion, ]
  estMean<-colMeans(a[6:10], na.rm=TRUE)
  dfMeans<-rbind(dfMeans,c(estacion,estMean))
}
dfMeans<-data.frame(dfMeans)
names(dfMeans)[1] <- 'Estacion'
Nombre<-rep(NA,8)
dfMeans<-cbind(Nombre,dfMeans)
for(myrow in 1:nrow(dfMeans)){#añadimos el nombre a la estacion
  dfMeans[myrow,]$Nombre<-asocNombresNum[asocNombresNum$ESTACION==dfMeans[myrow,]$Estacion,]$NombreEstacion
}


#Kmeans
#Una vez que tenemos rellenado el dataframe al completo escalamos los datos
myKmeans<-data.frame(scale(dfMeans[3:7]))
#Ahora vamos a calcular el numero optimo de clusters para el kMeans
#Para ello recurrimos al diagrama del codo.
#fviz_nbclust(myKmeans, kmeans, method = "wss")#No funciona ya que se generan mas centros de clusters que filas.
set.seed(123)
#Establecemos 4 clusters
kmeansResults<-kmeans(myKmeans, centers=4)
#Inercia entre grupos, nos dice la varianza entre los grupos, cuanto mayor mejor es la clasificación. Valor a maximizar.
kmeansResults$betweenss
#Inercia intragrupos, nos indica como de agrupados esta cada punto dentro de cada grupo, cuanto menor, mejor.
kmeansResults$withinss
#Inercia total intragrupos, es un valor a minimizar, cuanto menor es mejor.
kmeansResults$tot.withinss

estacion<-dfMeans$Nombre
numEst<-dfMeans$Estacion
grupoKmeans<-kmeansResults$cluster
datosRepresentacion<-data.frame(estacion, numEst, grupoKmeans)

ggplot(datosRepresentacion)+
  geom_point(mapping=aes(x=grupoKmeans, y=estacion), color=grupoKmeans, size=5)
#grupo 1 --> moratalaz
#grupo 2 --> juancarlos y hortaleza, madrid este
#grupo 3 --> estacion en plena m40
#grupo 4 --> estaciones externas fuera de la m40 y situadas en parques


#Hierarchical Clustering 
dfMeansNames <- dfMeans[,-1]
rownames(dfMeansNames) <- dfMeans[,1]
dd <- dist(dfMeansNames[,2:3], method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1, cex = 0.4)
grupoJerarquico <- cutree(hc, k = 4)
plot(hc, cex = 0.6) # plot tree
rect.hclust(hc, k = 4, border = 2:5) # add rectangle
datosRepresentacion<-cbind(datosRepresentacion,grupoJerarquico)
#En este dendograma y con la ayuda de la localizacion de las estaciones se ven relaciones
#Rojo --> Madrid sur colindante con la m40
#Verde --> Madrid exterior, fuera de la m40
#Azul oscuro --> Juancarlos I, parque en el exterior, poco efecto isla calor
#Azul claro --> Madrid norte, colindante a la m40

#Exportamos lo sresultados del clustering
write.csv2(datosRepresentacion, "Mapas/Clustering/aprox1.csv", row.names = FALSE)



###########################################FIN 1ª Aproximacion

##############2ª Aproximacion: cogemos las estaciones que tienen completos las magnitudes 83 y 86
rm(list=ls())

meteo21<-read.csv("Meteo/Diario/meteoDiario21_byDay.csv",sep=";", dec=",")
asocNombresNum<-read.csv("Meteo/estacion_distrito.csv",sep=";", dec=",")
#Puesto que hay muchas estaciones de las cuales tenemos bastantes nulos, vamos a realizar distintas
#aproximaciones dado el dataset que conseguimos del Ayuntamiento de Madrid

#Estaciones con las magnitudes 83 y 86 completas
vecEst<-c(102, 103, 106, 107, 109, 110, 112, 8, 24, 35, 36, 38, 54, 56, 58, 59)
sapply(meteo21, function(x) sum(is.na(x)))
dfSecond<-subset(meteo21,meteo21$Estacion %in% vecEst)
#Quitamos las magnitudes que no nos interesan
dfSecond<-dfSecond[,-c(6,7,10,11,12)]
sapply(dfSecond, function(x) sum(is.na(x)))#1,7% de NA para la 83 y 2% para la 86 


#Ahora por estacion vamos a sacar la media de sus magnitudes.
dfMeans<-NULL#dataframe con las medias de las magnitudes de cada estacion
for(estacion in unique(dfSecond$Estacion)){
  estMean<-NULL
  a<-dfSecond[dfSecond$Estacion==estacion, ]
  estMean<-colMeans(a[6:7],na.rm=TRUE)
  dfMeans<-rbind(dfMeans,c(estacion,estMean))
}
dfMeans<-data.frame(dfMeans)
names(dfMeans)[1] <- 'Estacion'
Nombre<-rep(NA,16)
dfMeans<-cbind(Nombre,dfMeans)
for(myrow in 1:nrow(dfMeans)){#añadimos el nombre a la estacion
  dfMeans[myrow,]$Nombre<-asocNombresNum[asocNombresNum$ESTACION==dfMeans[myrow,]$Estacion,]$NombreEstacion
}

#Una vez que tenemos rellenado el dataframe al completo escalamos los datos
myKmeans<-data.frame(scale(dfMeans[3:4]))

#Ahora vamos a calcular el numero optimo de clusters para el kMeans
#Para ello recurrimos al diagrama del codo.
fviz_nbclust(myKmeans, kmeans, method = "wss")#Recomendacion de 5 clusters
set.seed(123)
#Establecemos 5 clusters
kmeansResults<-kmeans(myKmeans, centers=5)
#Inercia entre grupos, nos dice la varianza entre los grupos, cuanto mayor mejor es la clasificación. Valor a maximizar.
kmeansResults$betweenss
#Inercia intragrupos, nos indica como de agrupados esta cada punto dentro de cada grupo, cuanto menor, mejor.
kmeansResults$withinss
#Inercia total intragrupos, es un valor a minimizar, cuanto menor es mejor.
kmeansResults$tot.withinss

estacion<-dfMeans$Nombre
numEst<-dfMeans$Estacion
grupoKmeans<-kmeansResults$cluster
datosRepresentacion<-data.frame(estacion, numEst,grupoKmeans)

ggplot(datosRepresentacion)+
  geom_point(mapping=aes(x=grupoKmeans, y=estacion), color=grupoKmeans, size=5)
#Grupo 1 --> parques de gran extension
#Grupo 2 --> situados en parques cercanos o bien m30 o bien m40
#Grupo 3 --> son principalmente estaciones situadas en zonas muy urbanas centro-sur, con poco efecto de zonas verdes a su alrededor, cercanas a industrias (madrid sur)
#grupo 4 --> zona urbana norte



#Hierarchical Clustering 
dfMeansNames <- dfMeans[,-1]
rownames(dfMeansNames) <- dfMeans[,1]
dd <- dist(dfMeansNames[,2:3], method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1, cex = 0.5)
grupoJerarquico <- cutree(hc, k = 5)
plot(hc, cex = 0.6) # plot tree
rect.hclust(hc, k = 5, border = 2:5) # add rectangle
datosRepresentacion<-cbind(datosRepresentacion,grupoJerarquico)


#Exportamos lo sresultados del clustering
write.csv2(datosRepresentacion, "Mapas/Clustering/aprox2.csv", row.names = FALSE)
#Conclusiones
#Rojo --> son estaciones que se encuentran en zonas verdes, en las que la masa arborea es más elevada que en el resto de zonas.
#verde --> chamberí, madrid urbana pero en la zona del norte.
#Azul oscuro --> estaciones que se situan en nucleos urbanos entre la m30 y m40, en zonas urbanas
#Azul claro --> se tratan de estaciones que están entre el centro y la m30, en zonas urbanas


###############################################Fin 2ª aproximacion
rm(list=ls())
###3ª aproximacion, ahor avamos a tener en cuenta las zonas verdes de cada una de las estaciones/distrito
#Estaciones con la magnitud 83 completa, y con datos sobre masas arboreas.
#Estas son las 12 estaciones que cumplen los requisitos
meteo21<-read.csv("Meteo/Diario/meteoDiario21_byDay.csv",sep=";", dec=",")
asocNombresNum<-read.csv("Meteo/estacion_distrito.csv",sep=";", dec=",")
vecEst<-c(8,18,24,36,38,54,58,59,102,103,106,107)
sapply(meteo21, function(x) sum(is.na(x)))
dfThird<-subset(meteo21,meteo21$Estacion %in% vecEst)
#Quitamos las magnitudes que no nos interesan
dfThird<-dfThird[,-c(6,7,9,10,11,12)]
sapply(dfThird, function(x) sum(is.na(x)))#0,7% de NA para la 83  

#Ahora por estacion vamos a sacar la media de sus magnitudes.
dfMeans<-NULL#dataframe con las medias de las magnitudes de cada estacion
for(estacion in unique(dfThird$Estacion)){
  estMean<-NULL
  a<-dfThird[dfThird$Estacion==estacion, ]
  estMean<-colMeans(a[6], na.rm=TRUE)
  dfMeans<-rbind(dfMeans,c(estacion,estMean))
}
dfMeans<-data.frame(dfMeans)
names(dfMeans)[1] <- 'Estacion'
Nombre<-rep(NA,12)
dfMeans<-cbind(Nombre,dfMeans)
for(myrow in 1:nrow(dfMeans)){#añadimos el nombre a la estacion
  dfMeans[myrow,]$Nombre<-asocNombresNum[asocNombresNum$ESTACION==dfMeans[myrow,]$Estacion,]$NombreEstacion
}


zonasVerdes<-read.csv("Zonas Verdes/masaArborea21.csv",sep=";", dec=",")
#Rellenamos con los datos sobre superficie arborea
superficie<-rep(NA,12)
dfMeans<-cbind(dfMeans,superficie)
for(myrow in 1:nrow(dfMeans)){
  mydistr<-asocNombresNum[asocNombresNum$ESTACION==dfMeans[myrow,]$Estacion,]$DISTRITO
  dfMeans[myrow,]$superficie<-zonasVerdes[zonasVerdes$NumDistr==mydistr,]$Superficiem2
}

dfMeans[dfMeans$Estacion==8,]$superficie<-350000 #Acorde a lo encontrado en google
#KMeans
#Una vez que tenemos rellenado el dataframe al completo escalamos los datos
myKmeans<-data.frame(scale(dfMeans[3:4]))

#Ahora vamos a calcular el numero optimo de clusters para el kMeans
#Para ello recurrimos al diagrama del codo.
fviz_nbclust(myKmeans, kmeans, method = "wss")#Recomendacion de 4 clusters
set.seed(123)
#Establecemos 3 clusters
kmeansResults<-kmeans(myKmeans, centers=4)
#Inercia entre grupos, nos dice la varianza entre los grupos, cuanto mayor mejor es la clasificación. Valor a maximizar.
kmeansResults$betweenss
#Inercia intragrupos, nos indica como de agrupados esta cada punto dentro de cada grupo, cuanto menor, mejor.
kmeansResults$withinss
#Inercia total intragrupos, es un valor a minimizar, cuanto menor es mejor.
kmeansResults$tot.withinss

estacion<-dfMeans$Nombre
numEst<-dfMeans$Estacion
grupoKmeans<-kmeansResults$cluster
datosRepresentacion<-data.frame(estacion, numEst, grupoKmeans)

ggplot(datosRepresentacion)+
  geom_point(mapping=aes(x=grupoKmeans, y=estacion), color=grupoKmeans, size=5)
#Grupo 1 --> proximos a zonas verdes, pero pegados a circunvalaciones, madird oeste-norte
#Grupo 2 --> parques de menor estension, madrid este-sur, cuatro caminos me chirria
#Grupo 3 --> zonas exteriores a las circunvalaciones, pero en zonas urbanas
#Grupo 4 --> zonas verdes, exteriores, no nucleo madrid

#Jerarquico
dfMeansNames <- dfMeans[,-1]
rownames(dfMeansNames) <- dfMeans[,1]
dd <- dist(dfMeansNames[,2:3], method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1, cex = 0.4)
grupoJerarquico <- cutree(hc, k = 4)
plot(hc, cex = 0.6) # plot tree
rect.hclust(hc, k = 4, border = 2:5) # add rectangle
datosRepresentacion<-cbind(datosRepresentacion,grupoJerarquico)
#Grupo 1 --> madrid norte cuatro caminos y este, zonas urbanas
#Grupo 2 --> zonax esteriores circunvalaciones
#Grupo 3 --> grandes zonas verdes, pero proximas a circunvalaciones
#grupo 4 --> zonas verdes de madrid norte


#Exportamos lo sresultados del clustering
write.csv2(datosRepresentacion, "Mapas/Clustering/aprox3.csv", row.names = FALSE)



##########CONTAMINACION
##############4ª Aproximacion: 
###############################################KMEANS
rm(list=ls())
contamina21<-read.csv("Contaminacion/Diario/contaminaDiario21_byDay.csv",sep=";", dec=",")
asocNombresNum<-read.csv("Contaminacion/estacion_distrito.csv",sep=";", dec=",")

#Puesto que hay muchas estaciones de las cuales tenemos bastantes nulos, vamos a realizar distintas
#aproximaciones dado el dataset que conseguimos del Ayuntamiento de Madrid

#En esta aproximación usaremos unicamente la magnitud 8, de todas las estaciones (24)

vecEst<-unique(contamina21$Estacion)
sapply(contamina21, function(x) sum(is.na(x)))
dfFourth<-subset(contamina21,contamina21$Estacion %in% vecEst)
sapply(dfFourth, function(x) sum(is.na(x)))
#Quitamos las magnitudes 8 y 9 que tienen muchos NA
dfFourth<-dfFourth[,-c(6,8,9,10)]


#Ahora por estacion vamos a sacar la media de sus magnitudes.
dfMeans<-NULL#dataframe con las medias de las magnitudes de cada estacion
for(estacion in unique(dfFourth$Estacion)){
  estMean<-NULL
  a<-dfFourth[dfFourth$Estacion==estacion, ]
  estMean<-colMeans(a[6], na.rm=TRUE)
  dfMeans<-rbind(dfMeans,c(estacion,estMean))
}
dfMeans<-data.frame(dfMeans)
names(dfMeans)[1] <- 'Estacion'
Nombre<-rep(NA,length(vecEst))
dfMeans<-cbind(Nombre,dfMeans)
for(myrow in 1:nrow(dfMeans)){#añadimos el nombre a la estacion
  dfMeans[myrow,]$Nombre<-asocNombresNum[asocNombresNum$ESTACION==dfMeans[myrow,]$Estacion,]$NomEst
}


#Kmeans
#Una vez que tenemos rellenado el dataframe al completo escalamos los datos
myKmeans<-data.frame(scale(dfMeans[3]))
#Ahora vamos a calcular el numero optimo de clusters para el kMeans
#Para ello recurrimos al diagrama del codo.
#fviz_nbclust(myKmeans, kmeans, method = "wss")#No funciona ya que se generan mas centros de clusters que filas.
set.seed(123)

kmeansResults<-kmeans(na.omit(myKmeans), centers=5)
#Inercia entre grupos, nos dice la varianza entre los grupos, cuanto mayor mejor es la clasificación. Valor a maximizar.
kmeansResults$betweenss
#Inercia intragrupos, nos indica como de agrupados esta cada punto dentro de cada grupo, cuanto menor, mejor.
kmeansResults$withinss
#Inercia total intragrupos, es un valor a minimizar, cuanto menor es mejor.
kmeansResults$tot.withinss

estacion<-dfMeans$Nombre
numEst<-dfMeans$Estacion
grupoKmeans<-kmeansResults$cluster
datosRepresentacion<-data.frame(estacion, numEst, grupoKmeans)

ggplot(datosRepresentacion)+
  geom_point(mapping=aes(x=grupoKmeans, y=estacion), color=grupoKmeans, size=5)

#Hierarchical Clustering 
dfMeansNames <- dfMeans[,-1]
rownames(dfMeansNames) <- dfMeans[,1]
dd <- dist(dfMeansNames, method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1, cex = 0.4)
grupoJerarquico <- cutree(hc, k = 5)
plot(hc, cex = 0.6) # plot tree
rect.hclust(hc, k = 5, border = 2:5) # add rectangle

datosRepresentacion<-cbind(datosRepresentacion,grupoJerarquico)


#Exportamos lo sresultados del clustering
write.csv2(datosRepresentacion, "Mapas/Clustering/aprox4.csv", row.names = FALSE)

###########################################FIN 4ª Aproximacion



#####################################Aproximación 5
rm(list=ls())
contamina21<-read.csv("Contaminacion/Diario/contaminaDiario21_byDay.csv",sep=";", dec=",")
asocNombresNum<-read.csv("Contaminacion/estacion_distrito.csv",sep=";", dec=",")

#En esta aproximación usaremos las magnitudes 8, 9 y 10, de las estaciones (8,24,38,47,48,50,56)

vecEst<-c(8,24,38,47,48,50,56)
sapply(contamina21, function(x) sum(is.na(x)))
dfFifth<-subset(contamina21,contamina21$Estacion %in% vecEst)
sapply(dfFifth, function(x) sum(is.na(x)))
#Quitamos las magnitudes 8 y 9 que tienen muchos NA
dfFifth<-dfFifth[,-c(6,10)]


#Ahora por estacion vamos a sacar la media de sus magnitudes.
dfMeans<-NULL#dataframe con las medias de las magnitudes de cada estacion
for(estacion in unique(dfFifth$Estacion)){
  estMean<-NULL
  a<-dfFifth[dfFifth$Estacion==estacion, ]
  estMean<-colMeans(a[6:8], na.rm=TRUE)
  dfMeans<-rbind(dfMeans,c(estacion,estMean))
}
dfMeans<-data.frame(dfMeans)
names(dfMeans)[1] <- 'Estacion'
Nombre<-rep(NA,length(vecEst))
dfMeans<-cbind(Nombre,dfMeans)
for(myrow in 1:nrow(dfMeans)){#añadimos el nombre a la estacion
  dfMeans[myrow,]$Nombre<-asocNombresNum[asocNombresNum$ESTACION==dfMeans[myrow,]$Estacion,]$NomEst
}


#Kmeans
#Una vez que tenemos rellenado el dataframe al completo escalamos los datos
myKmeans<-data.frame(scale(dfMeans[3:5]))
#Ahora vamos a calcular el numero optimo de clusters para el kMeans
#Para ello recurrimos al diagrama del codo.
#fviz_nbclust(myKmeans, kmeans, method = "wss")#No funciona ya que se generan mas centros de clusters que filas.
set.seed(123)

kmeansResults<-kmeans(na.omit(myKmeans), centers=4)
#Inercia entre grupos, nos dice la varianza entre los grupos, cuanto mayor mejor es la clasificación. Valor a maximizar.
kmeansResults$betweenss
#Inercia intragrupos, nos indica como de agrupados esta cada punto dentro de cada grupo, cuanto menor, mejor.
kmeansResults$withinss
#Inercia total intragrupos, es un valor a minimizar, cuanto menor es mejor.
kmeansResults$tot.withinss

estacion<-dfMeans$Nombre
numEst<-dfMeans$Estacion
grupoKmeans<-kmeansResults$cluster
datosRepresentacion<-data.frame(estacion, numEst, grupoKmeans)

ggplot(datosRepresentacion)+
  geom_point(mapping=aes(x=grupoKmeans, y=estacion), color=grupoKmeans, size=5)

#Hierarchical Clustering 
dfMeansNames <- dfMeans[,-1]
rownames(dfMeansNames) <- dfMeans[,1]
dd <- dist(dfMeansNames, method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc,hang = -1, cex = 0.4)
grupoJerarquico <- cutree(hc, k = 4)
plot(hc, cex = 0.6) # plot tree
rect.hclust(hc, k = 4, border = 2:5) # add rectangle

datosRepresentacion<-cbind(datosRepresentacion,grupoJerarquico)


#Exportamos lo sresultados del clustering
write.csv2(datosRepresentacion, "Mapas/Clustering/aprox5.csv", row.names = FALSE)

