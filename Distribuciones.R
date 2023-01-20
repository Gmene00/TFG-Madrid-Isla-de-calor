rm(list=ls())
#Analisis valores nulos y distribuciones
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
#Archivo con los datos a analizar
METEO21<-read.csv("Meteo/Diario/meteoDiario21_byDay.csv",sep=";", dec=",")
VALNA<-read.csv("Meteo/Diario/datosInvalidosDiarios.csv",sep=";", dec=",")#csv que contiene la tabla con 
#la distribucion de fallos por cada tupla estacion, mes, magnitud. Este fichero se crea en el
#script limpieza.r


#Fallos mensuales por estacion y magnitud, con esta gráfica se pretende saber como se reparten los
#datos nulos regogidos por la tupla(estación, magnitud, mes)

hist(x = VALNA$contInv,freq=FALSE, main = "Histograma de contInv")
lines(density(VALNA$contInv),col="red",lwd=2)

#Guardamos los datos para la grafica de valores NA
plotNA<-ggplot(data.frame(VALNA),aes(x=contInv)) +
  geom_density(fill="red", color="red")+
  xlim(-1,10)+
  labs(x = "NA (estación, magnitud, mes)", y = "Densidad") +
  ggtitle("Distribución de los valores NA")

#Guardamos los datos para la grafica de magnitud Velocidad del viento
plot81<-ggplot(METEO21,aes(x=X81), na.rm=TRUE) +
  geom_density(fill="#69b3a2", color="#e9ecef")+
  labs(x = "Velocidad del viento (m/s)", y = "Densidad") +
  ggtitle("Distribución de la velocidad del viento")

#Guardamos los datos para la grafica de magnitud direccion del viento
plot82<-ggplot(METEO21,aes(x=X82), na.rm=TRUE) +
  geom_density(fill="cadetblue3", color="cadetblue3")+
  labs(x = "Dirección del viento (0º-360º)", y = "Densidad") +
  ggtitle("Distribución de la dirección del viento")
#Predomina el viento del suroeste

#Guardamos los datos para la grafica de magnitud Temperatura
plot83<-ggplot(METEO21,aes(x=X83), na.rm=TRUE)+
  geom_density(fill="coral2", color="#e9ecef")+
  labs(x = "Temperatura (ºC)", y = "Densidad") +
  ggtitle("Distribución de la Temperatura")

#Guardamos los datos para la grafica de magnitud Humedad relativa
plot86<-ggplot(METEO21,aes(x=X86), na.rm=TRUE)+
  geom_density(fill="cyan2", color="#e9ecef")+
  labs(x = "Humedad (%)", y = "Densidad") +
  ggtitle("Distribución de la humedad relativa")

#Guardamos los datos para la grafica de magnitud Presion barometrica
plot87<-ggplot(METEO21,aes(x=X87), na.rm=TRUE)+
  geom_density(fill="darkgrey", color="#e9ecef")+
  labs(x = "Presión (mbar)", y = "Densidad") +
  ggtitle("Distribución de la presión barométrica")

#Guardamos los datos para la grafica de magnitud Radiacion solar
plot88<-ggplot(METEO21,aes(x=X88), na.rm=TRUE)+
  geom_density(fill="darkgoldenrod1", color="#e9ecef")+
  labs(x = "Radiación (W/m2)", y = "Densidad") +
  xlim(-20,400)+
  ggtitle("Distribución de la radiación solar")

#Guardamos los datos para la grafica de magnitud Precipitacion
plot89<-ggplot(METEO21,aes(x=X89), na.rm=TRUE)+
  geom_density(fill="cyan2", color="#e9ecef")+
  xlim(-3,15)+
  labs(x = "Precipitación (L/m2)", y = "Densidad") +
  ggtitle("Distribución de las precipitaciones")


#Mostramos todas las graficas
ggarrange(plotNA,plot81, plot82, plot83,plot86, plot87,plot88, plot89,
          ncol = 4, nrow = 2)



############################################
#Distribucion valores contaminacion

#Analisis valores nulos y outliers
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)
#Archivo con los datos a analizar
CONTAMINA21<-read.csv("Contaminacion/Diario/contaminaDiario21_byDay.csv",sep=";", dec=",")
VALNA<-read.csv("Contaminacion/Diario/datosInvalidosDiarios.csv",sep=";", dec=",")
#Vamos a calcular las distribuciones de los datos, una por cada magnitud. Primero sacamos
#todos los valores del DF principal con 2 bucles y los insertamos en una variable.
#Agrupamos en un dataframe y representamos esos valores en un diagrama de densidad.

#Fallos mensuales por estacion y magnitud, con esta gráfica se pretende saber como se reparten los
#datos nulos regogidos por la tupla(estación, magnitud, mes)


hist(x = VALNA$contInv,freq=FALSE, main = "Histograma de contInv")
lines(density(VALNA$contInv),col="red",lwd=2)

#Guardamos los datos para la grafica de los valores NA
plotNA<-ggplot(data.frame(VALNA),aes(x=contInv)) +
  geom_density(fill="red", color="red")+
  xlim(-1,10)+
  labs(x = "NA (estación, magnitud, mes)", y = "Densidad") +
  ggtitle("Distribución de los valores NA")


#Guardamos los datos para la grafica de magnitud Dioxido de Azufre
plot1<-ggplot(CONTAMINA21,aes(x=X1), na.rm=TRUE) +
  geom_density(fill="#69b3a2", color="#e9ecef")+
  labs(x = "Cantidad de SO2", y = "Densidad") +
  ggtitle("Distribución de Dioxido de Azufre")


#Guardamos los datos para la grafica de magnitud Dioxido de Nitrogeno
plot8<-ggplot(CONTAMINA21,aes(x=X8), na.rm=TRUE) +
  geom_density(fill="cadetblue3", color="cadetblue3")+
  labs(x = "Cantidad de NO2", y = "Densidad") +
  ggtitle("Distribución de Dioxido de Nitrogeno")

#Guardamos los datos para la grafica de magnitud Particulas 2.5um
plot9<-ggplot(CONTAMINA21,aes(x=X9), na.rm=TRUE)+
  geom_density(fill="coral2", color="#e9ecef")+
  labs(x = "Cantidad de PM 2.5", y = "Densidad") +
  ggtitle("Distribución Particulas 2.5 mm")

#Guardamos los datos para la grafica de magnitud Particulas 10um
plot10<-ggplot(CONTAMINA21,aes(x=X10), na.rm=TRUE)+
  geom_density(fill="cyan2", color="#e9ecef")+
  labs(x = "Cantidad de PM 10", y = "Densidad") +
  ggtitle("Distribución Particulas 10 mm")

#Guardamos los datos para la grafica de magnitud Ozono
plot14<-ggplot(CONTAMINA21,aes(x=X14), na.rm=TRUE)+
  geom_density(fill="darkgrey", color="#e9ecef")+
  labs(x = "Cantidad de PM O3", y = "Densidad") +
  ggtitle("Distribucion de Ozono")


#Mostramos todas las graficas
ggarrange(plotNA,plot1, plot8, plot9,plot10, plot14,
          ncol = 4, nrow = 2)

