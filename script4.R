library(dplyr)



set.seed(1.8)
Publicidad <- rnorm(100, mean=80, sd=10)
Ventas <- 30 + rnorm(100,mean=4,sd=0.3)*Publicidad + rnorm(100,mean = 0,sd=1)

cor(Publicidad, Ventas)




set.seed(1.8)
Publicidad <- rnorm(100, mean=80, sd=10)
Ventas <- 30 + rnorm(100,mean=4,sd=0.3)*Publicidad + rnorm(100,mean = 0,sd=1)

m1 = lm(Ventas ~ Publicidad)
summary(m1)




datos = read.csv("Ranking2018Comercio.csv",header=TRUE,sep=";", dec=",")

datos = datos %>%
  filter(VENTAS>0) %>%
  select(UTILIDAD,EMPLEADOS,VENTAS,TAMAÑO)
attach(datos)




pairs(datos[,1:3])




m2 = lm(UTILIDAD ~ EMPLEADOS + VENTAS)
summary(m2)



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



notas = read.csv("notas.csv",header=TRUE,sep=";")

notas = notas %>%
  mutate(APRUEBA = ifelse(FINAL>=70,1,0))

notas$NIVEL = as.factor(notas$NIVEL)
m7 <- glm(formula = APRUEBA ~ NIVEL + GENERO +CARRERA, family = binomial, data = notas)
summary(m7)
