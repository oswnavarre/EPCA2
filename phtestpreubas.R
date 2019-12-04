library(dplyr)
rank18com = read.csv("Ranking2018Comercio.csv",header=TRUE, dec=",", sep=";")
rank18com = rank18com %>%
  filter(TAMAÑO == "MICROEMPRESA") %>%
  select(ACTIVO)

attach(rank18com)


z.test(ACTIVO,sigma.x = desviacion)$conf.int
media = mean(ACTIVO)
desviacion = sd(ACTIVO)

error  = qnorm(0.975)*desviacion/sqrt(nrow(rank18com))
menor = media - error
mayor = media + error
menor
mayor
media

error  = qnorm(0.99)*desviacion/sqrt(nrow(rank18com))
menor = media - error
mayor = media + error
menor
mayor

?t.test
t.test(ACTIVO)$conf.int
