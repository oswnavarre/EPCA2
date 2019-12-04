library(ggplot2)
datos <- rnorm(10000, mean=200, sd=10)
media.pob<-mean(datos)

set.seed(1000)

muestras <- 100
cis <- matrix(0, nrow=muestras, ncol=5)
cis <- as.data.frame(cis)
colnames(cis) <- c("Muestra","Cae","Minimo","Media","Maximo")
cis[,1] <- factor(seq(1:muestras))
for(i in 1:muestras){
  muestra <- sample(datos, 100)
  media.spl <- mean(muestra)
  sd.spl <- sd(muestra)
  lower <- media.spl - 1.96*sd.spl/sqrt(muestras)
  upper <- media.spl + 1.96*sd.spl/sqrt(muestras)
  cis[i,3] <- lower
  cis[i,4] <- media.spl
  cis[i,5] <- upper
  cis[i,2] <- if((lower < media.pob & upper < media.pob) | (lower > media.pob & upper > media.pob)) "No" else "Si"
  
}

p <- ggplot(cis, aes(Muestra, colour = Cae)) +
  geom_linerange(aes(ymin = Minimo, ymax = Maximo)) + 
  geom_point(aes(y=Media),size=1) +
  geom_point(aes(y=Minimo),size=2, shape = 15) +
  geom_point(aes(y=Maximo),size=2, shape = 15) +
  geom_hline(yintercept = media.pob) +
  theme(legend.position = "none",
        axis.text.y =  element_blank(),
        axis.ticks.y = element_blank()) + 
  coord_flip() 
p 
