
library(tidyverse)
library(DescTools)
library(fGarch) 



set.seed(20)
df <- data.frame(
  normal_distrib = round(rnorm(n = 10000, mean = 0, sd = 1),2),
  exp_distrib = rexp(n = 10000, rate = .1),
  gamma_distrib = round(rgamma(n = 10000, shape = 2, scale = 2),2),
  beta_distrib = round(rbeta(n = 10000, shape1 = 8, shape2 = 2),2)

)



mean.beta = mean(df$beta_distrib)
median.beta = median(df$beta_distrib)
mode.beta = Mode(df$beta_distrib)

ggplot(data =df, aes(x=beta_distrib)) +
  geom_histogram(bins=20,fill="white",col="black") + 
  xlab("") + ylab("") +theme_void()


ggplot(data =df, aes(x=beta_distrib)) +
  geom_density() + geom_vline(xintercept=mean.beta) +
  geom_vline(xintercept=median.beta,colour="red") +
  geom_vline(xintercept=mode.beta[1],colour="blue") + 
  xlab("") + ylab("") +theme_void()




mean.gamma = mean(df$gamma_distrib)
median.gamma = median(df$gamma_distrib)
mode.gamma = Mode(df$gamma_distrib)

ggplot(data =df, aes(x=gamma_distrib)) +
  geom_histogram(bins=20,fill="white",col="black") + 
  xlab("") + ylab("") +theme_void()

ggplot(data =df, aes(x=gamma_distrib)) +
  geom_density() + geom_vline(xintercept=mean.gamma) +
  geom_vline(xintercept=median.gamma,colour="red") +
  geom_vline(xintercept=mode.gamma[1],colour="blue") + theme_void()


ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1), colour ="blue", size=1.20) +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  theme_void()


dt <- rnorm(10000, mean = 0, sd=1)
dt <- as.data.frame(dt)
names(dt) <- c("Tiempo")
mean.t = mean(dt$Tiempo)
median.t = median(dt$Tiempo)

ggplot(dt, aes(x=Tiempo)) + 
  geom_histogram(bins=20,colour="black", fill="white") +
  ylab("")  + xlab("")  + theme_void()

ggplot(dt, aes(x=Tiempo)) + 
  geom_density() +
  geom_vline(xintercept=mean.t) +
  geom_vline(xintercept=median.t,col="red") +
  ylab("")  + xlab("")  + theme_void()
