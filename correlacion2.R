library(ggplot2)
library(MASS)
library(gridExtra)
library(latex2exp)
library(grid)
library(pBrackets)
A <- seq(-1,1,0.25)
set.seed(1)

R = matrix(c(1,A[1],A[1],1), ncol = 2)
out <- mvrnorm(1000, mu = c(20,80), Sigma = R,
               empirical = TRUE)
out <- as.data.frame(out)
names(out) = c("Utilidad","Ventas")
p1 <-  ggplot(out, aes(x=Ventas,y=Utilidad))+ geom_point(colour="red") + xlab("") + ylab("") +stat_smooth(method="lm", se=FALSE)


R = matrix(c(1,A[2],A[2],1), ncol = 2)
out <- mvrnorm(1000, mu = c(20,80), Sigma = R,
               empirical = TRUE)
out <- as.data.frame(out)
names(out) = c("Utilidad","Ventas")
p2 <-  ggplot(out, aes(x=Ventas,y=Utilidad))+ geom_point(colour="red") + xlab("") + ylab("") +stat_smooth(method="lm", se=FALSE)

R = matrix(c(1,A[3],A[3],1), ncol = 2)
out <- mvrnorm(1000, mu = c(20,80), Sigma = R,
               empirical = TRUE)
out <- as.data.frame(out)
names(out) = c("Utilidad","Ventas")
p3 <-  ggplot(out, aes(x=Ventas,y=Utilidad))+ geom_point(colour="red") + xlab("") + ylab("") +stat_smooth(method="lm", se=FALSE)

R = matrix(c(1,A[4],A[4],1), ncol = 2)
out <- mvrnorm(1000, mu = c(20,80), Sigma = R,
               empirical = TRUE)
out <- as.data.frame(out)
names(out) = c("Utilidad","Ventas")
p4 <-  ggplot(out, aes(x=Ventas,y=Utilidad))+ geom_point(colour="red") + xlab("") + ylab("") +stat_smooth(method="lm", se=FALSE)

R = matrix(c(1,A[5],A[5],1), ncol = 2)
out <- mvrnorm(1000, mu = c(20,80), Sigma = R,
               empirical = TRUE)
out <- as.data.frame(out)
names(out) = c("Utilidad","Ventas")
p5 <-  ggplot(out, aes(x=Ventas,y=Utilidad))+ geom_point(colour="black") + xlab("") + ylab("") + stat_smooth(method="lm", se=FALSE) 

R = matrix(c(1,A[6],A[6],1), ncol = 2)
out <- mvrnorm(1000, mu = c(20,80), Sigma = R,
               empirical = TRUE)
out <- as.data.frame(out)
names(out) = c("Utilidad","Ventas")
p6 <-  ggplot(out, aes(x=Ventas,y=Utilidad))+ geom_point(colour="blue") + xlab("") + ylab("") +stat_smooth(method="lm", se=FALSE)

R = matrix(c(1,A[7],A[7],1), ncol = 2)
out <- mvrnorm(1000, mu = c(20,80), Sigma = R,
               empirical = TRUE)
out <- as.data.frame(out)
names(out) = c("Utilidad","Ventas")
p7 <-  ggplot(out, aes(x=Ventas,y=Utilidad))+ geom_point(colour="blue") + xlab("") + ylab("") +stat_smooth(method="lm", se=FALSE)

R = matrix(c(1,A[8],A[8],1), ncol = 2)
out <- mvrnorm(1000, mu = c(20,80), Sigma = R,
               empirical = TRUE)
out <- as.data.frame(out)
names(out) = c("Utilidad","Ventas")
p8 <-  ggplot(out, aes(x=Ventas,y=Utilidad))+ geom_point(colour="blue") + xlab("") + ylab("") +stat_smooth(method="lm", se=FALSE)

R = matrix(c(1,A[9],A[9],1), ncol = 2)
out <- mvrnorm(1000, mu = c(20,80), Sigma = R,
               empirical = TRUE)
out <- as.data.frame(out)
names(out) = c("Utilidad","Ventas")
p9 <-  ggplot(out, aes(x=Ventas,y=Utilidad))+ geom_point(colour="blue") + xlab("") + ylab("") +stat_smooth(method="lm", se=FALSE,colour="red")
g <- arrangeGrob(p1,p9, ncol=2)
ggsave("corr4.png",g)

g <- arrangeGrob(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow =3)
g <- arrangeGrob(p2,p3,p4,p6,p7,p8, nrow =3)
ggsave("corr2.png",g)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, rows=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, rows * ceiling(numPlots/rows)),
                     nrow = rows , ncol = ceiling(numPlots/rows))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$col,
                                      layout.pos.col = matchidx$row))
    }
  }
}

multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,rows =3)
last_plot()

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow =3)
last_plot()
g <- arrangeGrob(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow =3)
ggsave("corr.png",g)


set.seed(1.8)
Publicidad <- rnorm(100, mean=80, sd=10)
Ventas <- 30 + rnorm(100,mean=4,sd=0.3)*Publicidad + rnorm(100,mean = 0,sd=1)
datos <- cbind(Publicidad,Ventas)
datos <- as.data.frame(datos)
media.p <- mean(Publicidad)
media.v <- mean(Ventas)


ggplot(datos, aes(x=Publicidad,y=Ventas))+
  geom_point( ) + 
  geom_segment(aes(x=media.p, y=0, xend = media.p, yend = max(Ventas)), colour="blue",size =1.20, linetype ="dashed" ) +
  geom_segment(aes(x=min(Publicidad), y=media.v, xend = max(Publicidad), yend = media.v), colour="blue",size =1.20, linetype ="dashed" ) +
  geom_point(aes(x=media.p, y=media.v), colour="red",size=4)

m1 <- lm(Ventas~Publicidad)
coeficientes <-  coef(m1)
names(coeficientes) <- NULL
b0 <- coeficientes[1]
b1 <- coeficientes[2]
estimado = b0 + b1*64.76433

ggplot(datos, aes(x=Publicidad,y=Ventas))+
  geom_point( ) + stat_smooth(method="lm", se=FALSE,col ="red") +
  ylim(100,500) + geom_point(aes(x=64.76433,y=estimado),col="red") +
  annotate("text", label = TeX("$Y-\\hat{Y}$"), x = 67, y = 278, color = "blue") +geom_hline(yintercept = media.v,linetype="dashed") +
  annotate("text",label=TeX("$\\hat{Y}-\\bar{Y}$"),x=62.5,y=325,color="blue") +
  annotate("text",label=TeX("$Y-\\bar{Y}$"),x=57.5,y=315,color="blue") +
  geom_hline(yintercept=259.8485, linetype="dashed") +
  geom_hline(yintercept=289.2885, linetype="dashed") +
  theme_light()

grid.brackets(147, 207, 147, 184,h=0.02,col="blue",lwd=1,lty="dashed",type=2)
grid.brackets(147, 184, 147, 134,h=0.02,col="blue",lwd=1,lty="dashed",type=2)
grid.brackets(107, 207, 107, 134,h=0.02,col="blue",lwd=1,lty="dashed",type=2)

