datos <- read.csv("Ranking2018GuayasUtilidad.csv",header=TRUE,sep=";",dec=",")
str(datos)
plot(datos$ACTIVO,datos$UTILIDAD)

library(dplyr)



datos2 <- datos %>%
  filter(TAMAÑO == "MICROEMPRESA",CIUDAD== "GUAYAQUIL")

iqr = quantile(datos2$UTILIDAD,0.75)-quantile(datos2$UTILIDAD,0.25)

datos2 <- datos2 %>%
  filter(UTILIDAD < quantile(datos2$UTILIDAD,0.75) + 3*iqr & UTILIDAD > quantile(datos2$UTILIDAD,0.25)- 3*iqr)

iqr.ingresos = quantile(datos2$INGRESOS.POR.VENTA,0.75)-quantile(datos2$INGRESOS.POR.VENTA,0.25)

datos2 <- datos2 %>%
  filter(INGRESOS.POR.VENTA < quantile(datos2$INGRESOS.POR.VENTA,0.75) + 3*iqr.ingresos & INGRESOS.POR.VENTA > quantile(datos2$INGRESOS.POR.VENTA,0.25)- 3*iqr.ingresos)


boxplot(datos2$UTILIDAD)
boxplot(datos2$INGRESOS.POR.VENTA)
cor(datos2$INGRESOS.POR.VENTA , datos2$UTILIDAD)

datos3 <- read.csv("cap2_big4_size.csv",header=TRUE,sep=";",dec = ",")

str(datos3)

datos4 <- datos3 %>%
  filter(BIG4 == 1)
  
cor(datos4$UTILIDAD,datos4$VTAS)





R = matrix(c(1,.80,.6,.4,.2,  .8,1,.05,.1,.9,  .6,.05,1,.75,.3,   .4,.1,.75,1,.8,  0.2,0.9,0.3,0.8,1),nrow=5)
U = t(chol(R))
nvars = dim(U)[1]
numobs = 100
set.seed(1)
random.normal = matrix(rnorm(nvars*numobs,0,1), nrow=nvars, ncol=numobs);
X = U %*% random.normal
newX = t(X)
raw = as.data.frame(newX)
orig.raw = as.data.frame(t(random.normal))
names(raw) = c("Utilidad","Ventas")
cor(raw)
plot(head(raw, 100))
plot(head(orig.raw,100))


require(MASS)
out <- mvrnorm(1000, mu = c(30,40,80,90,30), Sigma = matrix(c(1,0.8,0.6,0.4,0.1,0.8,1,-0.3,-0.5,0.7,0.6,-0.3,1,0.9,0.3,0.4,-0.5,0.9,1,-0.8,0.1,0.7,0.3,-0.8,1), ncol = 5),
               empirical = FALSE)
cor(out)


A=matrix(c(1,0.8,0.6,0.4,0.1,0.8,1,-0.3,-0.5,0.7,0.6,-0.3,1,0.9,0.3,0.4,-0.5,0.9,1,-0.8,0.1,0.7,0.3,-0.8,1), ncol = 5)
origEig <- eigen(A)
origEig$values

newMat <- A
cholStatus <- try(u <- chol(A), silent = FALSE)
cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)

iter <- 0
while (cholError) {
  
  iter <- iter + 1
  cat("iteration ", iter, "\n")
  
  # replace -ve eigen values with small +ve number
  newEig <- eigen(newMat)
  newEig2 <- ifelse(newEig$values < 0, 0, newEig$values)
  
  # create modified matrix eqn 5 from Brissette et al 2007, inv = transp for
  # eig vectors
  newMat <- newEig$vectors %*% diag(newEig2) %*% t(newEig$vectors)
  
  # normalize modified matrix eqn 6 from Brissette et al 2007
  newMat <- newMat/sqrt(diag(newMat) %*% t(diag(newMat)))
  
  # try chol again
  cholStatus <- try(u <- chol(newMat), silent = TRUE)
  cholError <- ifelse(class(cholStatus) == "try-error", TRUE, FALSE)
}

out <- mvrnorm(1000, mu = c(30,40,80,90,30), Sigma = newMat,
               empirical = TRUE)
