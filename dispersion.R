
library(ggplot2)
CONJUNTO = c(rep("X",4),rep("Y",4))
valores = c(2,4,6,8,1,3,7,9)
datos = valores
datos = as.data.frame(datos)
datos =cbind(CONJUNTO, datos)


ggplot(datos, aes(x=valores,y=CONJUNTO)) + 
  geom_point(aes(col=CONJUNTO,size=2)) +
  scale_x_continuous(breaks=1:9) +
  geom_vline(xintercept=5, linetype="dashed") +
  theme(legend.position = "none",panel.background = element_rect(fill="white"))+
  ylab("") + xlab("") 
