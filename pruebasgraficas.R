library(ggplot2)
datos = read.csv("Ranking2018Comercio.csv",header = T,sep=";",dec=",")
str(datos)
ggplot(datos, aes(x=REGIÓN,fill=TAMAÑO)) + 
  geom_bar(stat = "count",position = "dodge") +
  xlab("") + ylab("Frecuencia")
  
library(dplyr)
library(tidyr)
tabla_reg <- datos %>%
  group_by(REGIÓN) %>%
  summarise(Frecuencia=n()) %>%
  mutate(Porcentaje = round(100*Frecuencia/sum(Frecuencia),2)
  ) %>%
  arrange(desc(Porcentaje))
print(tabla_reg)

datos$REGIÓN <- factor(datos$REGIÓN,
                             levels = c("COSTA", "SIERRA", "ORIENTE", "GALAPAGOS"))
ggplot(datos, aes(x=REGIÓN)) + 
  geom_bar(stat = "count",col="black",fill="white") +
  xlab("") + ylab("Frecuencia") 


tama.reg = datos %>% 
  group_by(TAMAÑO, REGIÓN)%>%
  summarise(n=n())%>%
  spread(TAMAÑO, n) %>%
  replace(., is.na(.), 0)


