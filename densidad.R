library(ggplot2)

dt <- rnorm(10000, mean = 80, sd=10)
dt <- as.data.frame(dt)
names(dt) <- c("Tiempo")



p1 <-ggplot(dt, aes(x=Tiempo)) + 
  geom_histogram(aes(y=..density..), bins=10,colour="black", fill="white") +
  ylab("") + geom_density(alpha=.2, fill="#FF6666")  + xlab("")

p2 <- ggplot(dt, aes(x=Tiempo)) + 
  geom_histogram(aes(y=..density..), bins=20,colour="black", fill="white") +
  ylab("") + geom_density(alpha=.2, fill="#FF6666") + xlab("")

p3 <- ggplot(dt, aes(x=Tiempo)) + 
  geom_histogram(aes(y=..density..), bins=30,colour="black", fill="white") +
  ylab("") + geom_density(alpha=.2, fill="#FF6666") + xlab("")

p4 <- ggplot(dt, aes(x=Tiempo)) + 
  geom_histogram(aes(y=..density..), bins=40,colour="black", fill="white") +
  ylab("") + geom_density(alpha=.2, fill="#FF6666") + xlab("")

p5 <- ggplot(dt, aes(x=Tiempo)) + 
  geom_histogram(aes(y=..density..), bins=50,colour="black", fill="white") +
  ylab("") + geom_density(alpha=.2, fill="#FF6666") + xlab("")

p6 <- ggplot(dt, aes(x=Tiempo)) + 
  geom_histogram(aes(y=..density..), bins=60,colour="black", fill="white") +
  ylab("") + geom_density(alpha=.2, fill="#FF6666") + xlab("")


grid.arrange(p1,p2,p3,p4,p5,p6)


normal_prob_area_plot <- function(lb, ub, mean = 0, sd = 1, limits = c(mean - 4 * sd, mean + 4 * sd)) {
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = dnorm(areax, mean = mean, sd = sd))
  (ggplot()
    + geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
                mapping = aes(x = x, y = y))
    + geom_ribbon(data = area, mapping = aes(x = x, ymin = ymin, ymax = ymax))
    + scale_x_continuous(limits = limits))
}

normal_prob_area_plot(-1,1)
normal_prob_area_plot(-3,3)

ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1), colour ="blue", size=1.5) + ylab("") +
  scale_y_continuous() + scale_x_continuous() +xlab("") +theme_void()


