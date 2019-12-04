


x <- c(3,7,9,5,6,2,1,10)



big4size <- read.csv("cap2_big4_size.csv",header=TRUE,sep=";",dec=",")
str(big4size)



install.packages("dplyr")



big4size <- big4size %>%
  mutate(
    ROS = UTILIDAD/VTAS,
    ROE = UTILIDAD/PAT
  )
str(big4size)



mean(big4size$ACTIVOS)
median(big4size$ACTIVOS)
library(DescTools)
Mode(big4size$ACTIVOS)



big4size %>%
  summarise(PROM.ACTIVOS = mean(ACTIVOS),
            PROM.UTILIDAD = mean(UTILIDAD),
            PROM.VTAS = mean(VTAS),
            MEDIAN.ACTIVOS = median(ACTIVOS),
            MEDIAN.UTILIDAD = median(UTILIDAD),
            MEDIAN.VTAS = median(VTAS)
  )



quantile(big4size$ACTIVOS, 0.25)



quantile(big4size$ACTIVOS, c(0.25,0.50,0.75))



quantile(big4size$ACTIVOS, seq(0.1,0.9, by = 0.1))



big4size %>%
  summarise(RANGO.ACTIVOS = max(ACTIVOS/1000000)-min(ACTIVOS/1000000),
            VARM.ACTIVOS = var(ACTIVOS/1000000),
            DESVM.ACTIVOS = sd(ACTIVOS/1000000),
            n=n()
  ) %>%
  mutate(VARP.ACTIVOS = VARM.ACTIVOS*((n-1)/n),
         DESVP.ACTIVOS = sqrt(VARP.ACTIVOS)) %>%
  select(RANGO.ACTIVOS, VARM.ACTIVOS, DESVM.ACTIVOS, VARP.ACTIVOS, DESVP.ACTIVOS)



audit_bolsa <- read.csv("audit_bolsa.csv",header=TRUE,sep=";",dec=",")

tabla_firma <- audit_bolsa %>%
  group_by(FIRMA) %>%
  summarise(Frecuencia=n()) %>%
  mutate(Porcentaje = round(100*Frecuencia/sum(Frecuencia),2)
  ) %>%
  arrange(desc(Porcentaje))
print(tabla_firma)



library(xlsx)
tabla_firma = as.data.frame(tabla_firma)
write.xlsx(tabla_firma, "tablas.xlsx", sheetName = "firmas", row.names = FALSE)



library(agricolae)

h2<-with(big4size,graph.freq(VTAS/1000000,plot=FALSE));

h2 = table.freq(h2)

h3 <- h2 %>%
  mutate(Clase = paste("[",Lower,",",Upper,")"),	
         "Marca de Clase"  =  Main,
         Frec. = Frequency,
         "Frec. Rel." = Percentage,
         "Frec. Acu." = CF,
         "Rel. Acu." = CPF )  %>%
  select(-c(1:7))



h3 = as.data.frame(h3)
write.xlsx(h3, "tablas.xlsx", sheetName = "frec_ventas", row.names = FALSE,append=TRUE)



library(tidyr)
rank2018 = read.csv("Ranking2018Guayas.csv",header=TRUE, sep=";",dec=",")

ciudad.tama = rank2018 %>% 
  group_by(CIUDAD, TAMAÑO)%>%
  summarise(n=n())%>%
  spread(TAMAÑO, n) %>%
  replace(., is.na(.), 0)



ciudad.tama.porc = rank2018 %>% 
  group_by(CIUDAD, TAMAÑO)%>%
  summarise(Porc = round(100*n()/nrow(rank2018),2)) %>%
  spread(TAMAÑO, Porc) %>%
  replace(., is.na(.), 0)



tipo.tama = rank2018 %>% 
  group_by(TIPO, TAMAÑO)%>%
  summarise(n=n())%>%
  spread(TAMAÑO, n) %>%
  replace(., is.na(.), 0)



tipo.tama.porc = rank2018 %>% 
  group_by(TIPO, TAMAÑO)%>%
  summarise(Porc = round(100*n()/nrow(rank2018),2)) %>%
  spread(TAMAÑO, Porc) %>%
  replace(., is.na(.), 0)










IPC = read.csv("IPC.csv",header = TRUE, dec=",", sep = ";")
head(IPC)



IPC2 = IPC %>%
  gather(key = "MES", value = "IPC", 2:13)

head(IPC2)



IPC3 = IPC2 %>%
  spread(key = "MES", value = "IPC")

head(IPC3)



ggplot(big4size, aes(x= VTAS/1000000)) + 
  geom_histogram(bins=12, color= "red", fill="blue" ) + 
  theme_light()



ggplot(big4size, aes(x= VTAS/1000000)) + 
  geom_histogram(bins=12, color= "red",  fill="blue" ) + 
  xlab("Ventas en Millones de Dólares") + ylab("Frecuencia") +
  theme_light()



rank2018 = read.csv("Ranking2018Guayas.csv",header=TRUE, sep=";",dec=",")

ggplot(rank2018, aes(x=VENTAS/1000, fill=TAMAÑO)) + 
  geom_histogram(alpha=0.3, color="black",bins=10, binwidth = 150) +
  scale_x_continuous(breaks = seq(0,1350,150)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.background = element_rect(fill="white")) +
  xlab("Ventas en Miles") + ylab("Frecuencia")



ggplot(rank2018, aes(x=VENTAS/1000000)) + 
  geom_histogram(alpha=0.3, color="black",bins = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.background = element_rect(fill="white")) +
  xlab("Ventas en Miles") + ylab("Frecuencia") +
  facet_grid(. ~ TAMAÑO)



rank2018com = read.csv("Ranking2018Comercio.csv",header = T,sep=";",dec=",")
str(rank2018com)



tabla_reg <- rank2018com %>%
  group_by(REGIÓN) %>%
  summarise(Frecuencia=n()) %>%
  mutate(Porcentaje = round(100*Frecuencia/sum(Frecuencia),2)
  ) %>%
  arrange(desc(Porcentaje))
print(tabla_reg)



ggplot(rank2018com, aes(x=REGIÓN)) + 
  geom_bar(stat = "count",col="black",fill="white") +
  xlab("") + ylab("Frecuencia")



rank2018com$REGIÓN <- factor(rank2018com$REGIÓN,
                             levels = c("COSTA", "SIERRA", "ORIENTE", "GALAPAGOS"))



ggplot(rank2018com, aes(x=REGIÓN)) + 
  geom_bar(stat = "count",col="black",fill="white") +
  xlab("") + ylab("Frecuencia")



tama.reg = rank2018com %>% 
  group_by(TAMA, REGIÓN)%>%
  summarise(n=n())%>%
  spread(TAMA, n) %>%
  replace(., is.na(.), 0)

print(tama.reg)



ggplot(rank2018com, aes(x=REGIÓN,fill=TAMA)) + 
  geom_bar(stat = "count",position = "dodge") +
  xlab("") + ylab("Frecuencia")



ggplot(rank2018com, aes(x=REGIÓN,fill=TAMA)) + 
  geom_bar(stat = "count",position = "dodge") +
  scale_fill_discrete(name="Tamaño") + 
  xlab("") + ylab("Frecuencia")



ggplot(rank2018, aes(TAMAÑO, VENTAS/1000)) + 
  geom_boxplot() + xlab("Tamaño de las empresas") +
  ylab("Ventas en Miles de Dólares") +theme_light()



ggplot(subset(rank2018, TAMAÑO == "MICROEMPRESA"), aes(TAMAÑO, VENTAS/1000)) + 
  geom_boxplot() + xlab("") +
  ylab("Ventas en Miles de Dólares") +theme_light()