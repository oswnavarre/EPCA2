library(dplyr)
library(ggplot2)
datos = read.csv("Ranking2018Comercio.csv",header=TRUE,sep=";", dec=",")

datos = datos %>%
  filter(VENTAS>0) %>%
  select(UTILIDAD,EMPLEADOS,VENTAS,TAMAÑO)
attach(datos)

pairs(datos[,1:3])


ggplot(datos, aes(x = VENTAS/1000, y = UTILIDAD/1000))+
  geom_point(aes(color = TAMAÑO))

ggplot(datos, aes(x = VENTAS/1000, y = EMPLEADOS))+
  geom_point(aes(color = TAMAÑO))


ggplot(subset(datos,TAMAÑO=="MEDIANA"), aes(y = VENTAS/1000, x = EMPLEADOS))+
  geom_point()



rank18com = read.csv("Ranking2018Comercio.csv",header=TRUE, 
                     dec=",", sep=";")

rank18com.peq = rank18com %>%
  filter(TAMAÑO == "MICROEMPRESA") %>%
  select(ACTIVO)

attach(rank18com.peq)

mean(ACTIVO)
