setwd("C:/Users/onava_000/OneDrive/libro_mc/estadistica")
datos = read.csv("cap2_big4_size.csv",header=TRUE,sep=";",dec=",")
str(datos)
library(dplyr)
library(ggplot2)
datos$BIG4 <- as.factor(datos$BIG4)

recode(datos$BIG4, "1" = "Sí", "0" ="No")

datos = datos %>%
  mutate(BIG4.fct = if_else(BIG4 == 0, "No", "Sí"))

ggplot(datos, aes(x=PAT/1000000, fill=BIG4.fct)) + geom_histogram(bins = 12)

datos1 = read.csv("Ranking2018Guayas.csv",header=TRUE,sep=";")
str(datos1)
ggplot(datos1, aes(x=VENTAS/1000, fill=TAMAÑO)) + 
  geom_histogram(alpha=0.3, color="black",bins=9, binwidth = 150) +
  scale_x_continuous(breaks = seq(0,1350,150)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

min(datos1$VENTAS)
max(datos1$VENTAS)
