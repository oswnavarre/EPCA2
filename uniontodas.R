library(dplyr)
library(ggplot2)
agricultura = read.csv("rankingagricultura.csv",header=TRUE,sep=";",dec=",")
comercio = read.csv("rankingcomercio.csv",header=TRUE,sep=";",dec=",")
construccion = read.csv("rankingconstruccion.csv",header=TRUE,sep=";",dec=",")
inmobiliaria = read.csv("rankinginmobiliaria.csv",header=TRUE,sep=";",dec=",")
manufactura = read.csv("rankingmanufactura.csv",header=TRUE,sep=";",dec=",")

datos = rbind(agricultura,comercio,construccion,inmobiliaria,manufactura)
rm("agricultura","comercio","construccion","inmobiliaria","manufactura")
?top_n

datos2 = top_n(datos,1000,UTILIDAD)

tendencia = datos2 %>%
  summarise(
    promedio = mean(UTILIDAD),
    promediovtas = mean(VENTAS),
    desv = sd(UTILIDAD),
    desventas = sd(VENTAS)
  )

tendenciatodos = datos %>%
  summarise(
    promedio = mean(UTILIDAD),
    promediovtas = mean(VENTAS),
    desv = sd(UTILIDAD),
    desventas = sd(VENTAS)
  )

tendencia2 =datos2 %>%
  group_by(SECTOR_PROD) %>%
  summarise(
    promedio = mean(UTILIDAD),
    promediovtas = mean(VENTAS),
    desv = sd(UTILIDAD),
    desventas = sd(VENTAS)
  )

tendencia2todos =datos %>%
  group_by(SECTOR_PROD) %>%
  summarise(
    promedio = mean(UTILIDAD),
    promediovtas = mean(VENTAS),
    desv = sd(UTILIDAD),
    desventas = sd(VENTAS)
  )

write.csv(datos, file = "todas2018.csv")
