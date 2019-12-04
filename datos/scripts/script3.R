

big4size <- big4size %>% 
  mutate(
    Big4 = ifelse(BIG4==1, "Sí","No")
  )



ggplot(big4size, aes(x=VTAS/1000, fill=Big4)) + 
  geom_histogram(alpha=0.3, color="black",bins=10, binwidth = 200000) +
  scale_x_continuous(breaks = seq(0,2000000,200000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Ventas en Miles") + ylab("Frecuencia")



ggplot(big4size, aes(Big4, VTAS/1000)) + 
  geom_boxplot() + xlab("Tipo de Firma") +
  ylab("Ventas en Miles de Dólares")



rank18com = read.csv("Ranking2018Comercio.csv",header=TRUE, dec=",", sep=";")

rank18com.Micro = rank18com %>%
  filter(TAMA == "MICROEMPRESA") %>%
  select(ACTIVO)

attach(rank18com.Micro)

media = mean(ACTIVO)
desviacion = sd(ACTIVO)

error  = qnorm(0.975)*desviacion/sqrt(nrow(rank18com.Micro))

menor = media - error
mayor = media + error
menor
mayor



error  = qnorm(0.99)*desviacion/sqrt(nrow(rank18com.Micro))
menor = media - error
mayor = media + error
menor
mayor



library(BSDA)
z.test(ACTIVO,sigma.x = desviacion)$conf.int



z.test(ACTIVO,sigma.x = desviacion, conf.level = 0.98)$conf.int



t.test(ACTIVO)$conf.int



t.test(ACTIVO, conf.level = 0.90 )$conf.int



rank18com = read.csv("Ranking2018Comercio.csv",header=TRUE,dec=",", sep=";")

x = nrow(rank18com[which(rank18com$TAMA == "GRANDE"), ])
n = nrow(rank18com)



prop.test(x,n)$conf.int



rank18com.Peq = rank18com %>%
  filter(TAMA == "PEQUEÑA") %>%
  select(ACTIVO)
act.Peq = rank18com.Peq$ACTIVO

rank18com.Micro = rank18com %>%
  filter(TAMA == "MICROEMPRESA") %>%
  select(ACTIVO)
act.Micro = rank18com.Micro$ACTIVO



library(BSDA)
z.test(x=act.Peq, sigma.x = sd(act.Peq), 
       y=act.Micro, sigma.y = sd(act.Micro))$conf.int



z.test(x=act.Peq, sigma.x = sd(act.Peq), 
       y=act.Micro, sigma.y = sd(act.Micro), 
       conf.level = 0.90)$conf.int



t.test(x=act.Peq, y=act.Micro, var.equal = FALSE,
       conf.level = 0.95)$conf.int



t.test(x=act.Peq, y=act.Micro, var.equal = FALSE,
       conf.level = 0.98)$conf.int



t.test(x=act.Peq, y=act.Micro, var.equal = TRUE,
       conf.level = 0.95)$conf.int



t.test(x=act.Peq, y=act.Micro, var.equal = TRUE,
       conf.level = 0.98)$conf.int



n = nrow(rank18com)
x1 = length(act.Micro)
x2 = length(act.Peq)



prop.test(c(x1,x2),c(n,n))$conf.int



prop.test(c(x1,x2),c(n,n), conf.level = 0.99)$conf.int




desviacion = sd(act.Micro)
z.test(act.Micro, mu = 40000, sigma.x = desviacion, conf.level = 0.95)



z.test(act.Micro, mu = 40000, sigma.x = desviacion,alternative = "less",  conf.level = 0.95)



t.test(act.Peq, mu = 250000,alternative = "greater",  conf.level = 0.95)



n = nrow(rank18com)
x1 = length(act.Micro)
prop.test(x1, n, p = 0.50, alternative = "greater",  conf.level = 0.95)



n = nrow(rank18com)
x2 = length(act.Peq)
prop.test(x2, n, p = 0.25, alternative = "less",  conf.level = 0.98)



z.test(x=act.Peq, sigma.x = sd(act.Peq), 
       y=act.Micro, sigma.y = sd(act.Micro),
       alternative = "greater", conf.level = 0.90)



t.test(x=act.Peq, y=act.Micro, var.equal = FALSE,
       conf.level = 0.95)



t.test(x=act.Peq, y=act.Micro, var.equal = TRUE,
       conf.level = 0.95)



construc = read.csv("rankingconstruccion.csv",header=TRUE, 
                    dec=",", sep=";")

manufact = read.csv("rankingmanufactura.csv",header=TRUE, 
                    dec=",", sep=";")

GRAN.CONSTRUC = nrow(construc[which(construc$TAMAÑO == "GRANDE"), ])
N.CONSTRUC = nrow(construc)

GRAN.MANUFACT = nrow(manufact[which(manufact$TAMAÑO == "GRANDE"), ])
N.MANUFACT = nrow(construc)



prop.test(x=c(GRAN.CONSTRUC,GRAN.MANUFACT),n=c(N.CONSTRUC,N.MANUFACT), 
          alternative = "greater")



