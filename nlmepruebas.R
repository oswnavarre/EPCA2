install.packages("nlme")
library(dplyr)


datos = read.csv("Ranking2018Comercio.csv",header=TRUE,sep=";", dec=",")

datos = datos %>%
  filter(VENTAS>0) %>%
  select(UTILIDAD,EMPLEADOS,VENTAS,TAMAÑO)
attach(datos)



m2 = lm(UTILIDAD ~ EMPLEADOS + VENTAS)
summary(m2)

m3 = lm(UTILIDAD ~ EMPLEADOS + VENTAS + TAMAÑO)
summary(m3)

m4 = lm(UTILIDAD ~ -1 + EMPLEADOS + VENTAS + TAMAÑO)
summary(m4)

library(nlme)
m5=lme(UTILIDAD ~ EMPLEADOS+VENTAS,random = ~1|TAMAÑO)
summary(m5)

m6=lme(UTILIDAD ~ -1+EMPLEADOS+VENTAS,random = ~1|TAMAÑO)
summary(m6)


anova(m2,m3,m4)


summary(anova.lme(m5,m6))





glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = Smarket)