library(ggplot2)
library(latex2exp)


ggplot(data.frame(x = c(490, 510)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(500, 1), col="red",size=1.5) +
  stat_function(fun = dnorm, args = list(498.04, 1), col="blue") +
  stat_function(fun = dnorm, args = list(501.96, 1), col="blue")  + 
  geom_vline(xintercept = 498.04, linetype="dotted", color = "blue", size=1) + 
  geom_vline(xintercept = 501.96, linetype="dotted", color = "blue", size=1) + 
  geom_area(stat = "function", fun = dnorm, args = list(501.96, 1),
            fill = "lightblue", xlim = c(500, 500.98), alpha=0.4) +
  geom_area(stat = "function", fun = dnorm, args = list(500, 1), 
            fill = "lightblue", xlim = c(500.98, 501.96), alpha=0.4) +
  geom_area(stat = "function", fun = dnorm, args = list(498.04, 1), 
            fill = "lightblue", xlim = c(499.02, 500), alpha=0.4) +
  geom_area(stat = "function", fun = dnorm, args = list(500, 1), 
            fill = "lightblue", xlim = c(498.04, 499.02), alpha=0.4) +
  geom_area(stat = "function", fun = dnorm, args = list(500, 1), 
            fill = "lightgreen", xlim = c(490, 498.04), alpha=0.8) +
  geom_area(stat = "function", fun = dnorm, args = list(500, 1), 
            fill = "lightgreen", xlim = c(501.96, 505), alpha=0.8) +
  xlab("") + ylab("") + theme_void()


ggplot(data.frame(x = c(490, 510)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(500, 1), col="red",size=1.5,xlim = c(493, 507)) +
  stat_function(fun = dnorm, args = list(498.04, 1), col="blue",xlim = c(493, 507), linetype = "dashed") +
  geom_area(stat = "function", fun = dnorm, args = list(500, 1), 
            fill = "lightgreen", xlim = c(493, 498.04), alpha=0.8) +
  xlab("") + ylab("") + theme_void()

ggplot(data.frame(x = c(490, 510)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(500, 1), col="red",size=1.5,xlim = c(493, 507)) +
  stat_function(fun = dnorm, args = list(501.96, 1), col="blue",xlim = c(493, 507),linetype = "dashed") +
  geom_area(stat = "function", fun = dnorm, args = list(500, 1), 
            fill = "lightgreen", xlim = c(501.96, 507), alpha=0.8) +
  xlab("") + ylab("") + theme_void()

ggplot(data.frame(x = c(490, 510)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(500, 1), col="red",size=1.5,xlim = c(493, 507)) +
  stat_function(fun = dnorm, args = list(501.96, 1), col="blue",xlim = c(493, 507),linetype = "dashed") +
  stat_function(fun = dnorm, args = list(498.04, 1), col="blue",xlim = c(493, 507),linetype = "dashed") +
  geom_area(stat = "function", fun = dnorm, args = list(500, 1), 
            fill = "lightgreen", xlim = c(493, 498.04), alpha=0.8) +
  geom_area(stat = "function", fun = dnorm, args = list(500, 1), 
            fill = "lightgreen", xlim = c(501.96, 507), alpha=0.8) +
  xlab("") + ylab("") + theme_void()

ggplot(data.frame(x = c(490, 510)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(500, 1), col="red",size=1.5,xlim = c(493, 507)) +
  stat_function(fun = dnorm, args = list(501.96, 1), col="blue",xlim = c(493, 507),linetype = "dashed") +
  geom_area(stat = "function", fun = dnorm, args = list(501.96, 1), 
            fill = "lightgreen", xlim = c(500.8, 507), alpha=0.8) +
  xlab("") + ylab("") + theme_void()

ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1), colour ="blue", size=1.20) +
  ylab("") +   scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = c(0),labels = parse(text = TeX('$\\mu$') )) +
  xlab("") + geom_hline(yintercept = -0.01, colour ="black") + geom_vline(xintercept = 0, linetype ="dashed", colour ="lightblue",size =1.5) +theme(axis.text = element_text(size = 15), panel.background = element_rect(fill="white")) 


ggplot(data.frame(x = c(-6, 6)), aes(x)) + 
  mapply(function(mean, sd, col) {
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd), col = col)
  }, 
  # enter means, standard deviations and colors here
  mean = c(0, -2, 2), 
  sd = c(1, 1, 1), 
  col = c('red', 'blue', 'green')
  ) + ylab("") +   scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = c(-2,0,2),labels = c(parse(text = TeX('$\\mu_1$')), parse(text = TeX('$\\mu_2$')), parse(text = TeX('$\\mu_3$'))  ) ) +
  xlab("") + geom_hline(yintercept = -0.01, colour ="black") + geom_vline(xintercept = c(-2,0,2), linetype ="dashed", colour ="lightblue",size =1.5) +theme(axis.text = element_text(size = 15), panel.background = element_rect(fill="white")) 

ggplot(data.frame(x = c(-6, 6)), aes(x)) + 
  mapply(function(mean, sd, col) {
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd), col = col,size=1.20)
  }, 
  # enter means, standard deviations and colors here
  mean = c(0,0, 0, 0, 0), 
  sd = c(2,1.5,1, 0.5, 0.40), 
  col = c('purple','black','red', 'blue', 'green')
  ) + ylab("") +   scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL) +
  xlab("") + geom_hline(yintercept = -0.01, colour ="black") + theme(panel.background = element_rect(fill="white")) 
library(ggplot2)

ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1), colour ="blue", size=1.20) +
  ylab("") +   scale_y_continuous(breaks = NULL) + 
  scale_x_continuous(breaks = c(-1,0,1),labels = c(parse(text = TeX('$\\mu - 1\\sigma$')),parse(text = TeX('$\\mu $') ),parse(text = TeX('$\\mu + 1 \\sigma$') ) )) +
  xlab("") + geom_hline(yintercept = 0, colour ="black") +
  geom_segment(aes(x = 0, y=0, xend=0, yend=dnorm(0,mean=0,sd=1)), colour ="black",linetype="dashed") +
  theme(axis.text = element_text(size = 15), 
        panel.background = element_rect(fill="white"),axis.ticks = element_blank())  +
  geom_segment(aes(x=-1, y=0, xend = -1, yend = dnorm(mean=0, sd=1, x=-1)), colour="green",size =1.20, linetype ="dashed" ) +
  geom_segment(aes(x=1, y=0, xend = 1, yend = dnorm(mean=0, sd=1, x=1)), colour="green",size =1.20, linetype ="dashed" ) +
  geom_area(stat = "function", fun = dnorm, args = list(0, 1), 
            fill = "lightgreen", xlim = c(-1, 1), alpha=0.8)


ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1), colour ="blue", size=1.20) +
  ylab("") +   scale_y_continuous(breaks = NULL) + 
  scale_x_continuous(breaks = c(-2,0,2),labels = c(parse(text = TeX('$\\mu - 2\\sigma$')),parse(text = TeX('$\\mu $') ),parse(text = TeX('$\\mu + 2 \\sigma$') ) )) +
  xlab("") + geom_hline(yintercept = 0, colour ="black") +
  geom_segment(aes(x = 0, y=0, xend=0, yend=dnorm(0,mean=0,sd=1)), colour ="black",linetype="dashed") +
  theme(axis.text = element_text(size = 15), 
        panel.background = element_rect(fill="white"),axis.ticks = element_blank())  +
  geom_segment(aes(x=-2, y=0, xend = -2, yend = dnorm(mean=0, sd=1, x=-2)), colour="blue",size =1.20, linetype ="dashed" ) +
  geom_segment(aes(x=2, y=0, xend = 2, yend = dnorm(mean=0, sd=1, x=2)), colour="blue",size =1.20, linetype ="dashed" ) +
  geom_area(stat = "function", fun = dnorm, args = list(0, 1), 
            fill = "lightblue", xlim = c(-2, 2), alpha=0.6)

ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1), colour ="blue", size=1.20) +
  ylab("") +   scale_y_continuous(breaks = NULL) + 
  scale_x_continuous(breaks = c(-3,0,3),labels = c(parse(text = TeX('$\\mu - 3\\sigma$')),parse(text = TeX('$\\mu $') ),parse(text = TeX('$\\mu + 3 \\sigma$') ) )) +
  xlab("") + geom_hline(yintercept = 0, colour ="black") +
  geom_segment(aes(x = 0, y=0, xend=0, yend=dnorm(0,mean=0,sd=1)), colour ="black",linetype="dashed") +
  theme(axis.text = element_text(size = 15), 
        panel.background = element_rect(fill="white"),axis.ticks = element_blank())  +
  geom_segment(aes(x=-3, y=0, xend = -3, yend = dnorm(mean=0, sd=1, x=-3)), colour="red",size =1.20, linetype ="dashed" ) +
  geom_segment(aes(x=3, y=0, xend = 3, yend = dnorm(mean=0, sd=1, x=3)), colour="red",size =1.20, linetype ="dashed" ) +
  geom_area(stat = "function", fun = dnorm, args = list(0, 1), 
            fill = "red", xlim = c(-3, 3), alpha=0.2)


ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  ylab("") +   scale_y_continuous(breaks = NULL,limits = c(0,0.8))  + scale_x_continuous(breaks=c(0)) +
  stat_function(fun = dt, n = 10000, args = list(df = 1, ncp=0), colour ="red", size=1) +
  stat_function(fun = dt, n = 10000, args = list(df = 9, ncp=0), colour ="green", size=1) + xlab("") + 
  theme(axis.text = element_text(size = 15), panel.background = element_rect(fill="white"),axis.ticks = element_blank()) +
 geom_hline(yintercept = 0, colour ="black") 
  
ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  ylab("") +   scale_y_continuous(breaks = NULL,limits = c(0,0.4))  + scale_x_continuous(breaks=c(0)) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1), colour ="blue", size=1) +
  stat_function(fun = dt, n = 10000, args = list(df = 1, ncp=0), colour ="red", size=1) +
  stat_function(fun = dt, n = 10000, args = list(df = 9, ncp=0), colour ="green", size=1) + xlab("") + 
  theme(axis.text = element_text(size = 15), panel.background = element_rect(fill="white"),axis.ticks = element_blank()) +
  geom_hline(yintercept = 0, colour ="black") 
