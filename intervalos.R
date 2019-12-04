mean <- 200
sd <- 20
datos <- rnorm(n=1000,mean = 200, sd = 20)

muestras <- 100
cis <- matrix(0, nrow = muestras, ncol = 3)

for(i in 1:muestras){
  muestra <- sample(datos, 100)
  muestra.m <- mean(muestra)
  muestra.sd <- sd(muestra)
  minimo <- muestra.m - 1.96*muestra.sd/sqrt(100)
  maximo <- muestra.m + 1.96*muestra.sd/sqrt(100)
  cis[i,1] <- as.integer(i)
  cis[i,2] <- minimo
  cis[i,3] <- maximo
}


cis <- as.data.frame(cis)
colnames(cis) <- c("muestra","low","up")
cis$muestra <- as.integer(cis$muestra)

library(ggplot2)
library(reshape2)

# Reshape data to long form.
mdat <- melt(cis,  id="muestra")


ggplot(data=mdat, aes(x=muestra, y =value)) + 
  geom_linerange() 


x <- seq(1:10)
n <- length(x)
yone <- 2 * runif(n)
ytwo <- runif(n)
ythree <- ytwo * 0.2
yfour <- ytwo * 2

df <- data.frame(x, yone, ytwo, ythree, yfour); df
mdat <- melt(df, id.vars=c("x", "ythree", "yfour"))
