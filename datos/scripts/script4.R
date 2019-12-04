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
  select(UTILIDAD,EMPLEADOS,VENTAS,TAMA)
attach(datos)



pairs(datos[,1:3])



m3 = lm(UTILIDAD ~ EMPLEADOS + VENTAS + TAMA)
summary(m3)



m4 = lm(UTILIDAD ~ -1 + EMPLEADOS + VENTAS + TAMA)
summary(m4)


datos =read.csv("cap2_big4_size.csv",header=TRUE,dec=",",sep=";")

ggplot(datos,aes(x=UTILIDAD, y=BIG4)) + geom_point()



ggplot(datos,aes(x=UTILIDAD, y=BIG4)) + geom_point() + 
  geom_smooth(method = "lm",se=FALSE)



notas = read.csv("notas.csv",header=TRUE,sep=";")

str(notas)



notas$NIVEL = as.factor(notas$NIVEL)

notas = notas %>%
  mutate(APRUEBA = ifelse(FINAL>=70,1,0))

str(notas)



m5 = glm(formula = APRUEBA ~ NIVEL, family = binomial, data = notas)

summary(m5)



m6 = glm(formula = APRUEBA ~ GENERO + NIVEL , family = binomial, data = notas)

summary(m6)


m7 = glm(formula = APRUEBA ~ -1 + GENERO + NIVEL + CARRERA , family = binomial, data = notas)

summary(m7)