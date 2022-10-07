
##################################################
library(tidyverse)
# 1
set.seed(123)
beta0<- rep(0,200)
beta1<- rep(1,200)
beta2<- rep(1,200)
beta3<- rep(10,200) # c(-10,0,10) # antagonism, no interaction, or synergism
x1<- runif(200,min = 0, max = 1)
x2 <- runif(200,min = 0, max = 1)
e <- rnorm(200)

y = beta0 + beta1*x1 + beta2*x2 + beta3*(x1*x2) + e

observed<- tibble(y,x1,x2)
observed

mod <- lm(y~x1*x2,observed)
observed$z <- predict(mod,observed)

ggplot(observed,aes(x1,x2,color=factor(z)))+
  geom_point()+
  geom_smooth()+
  theme_bw() +
  theme(legend.position = "none")


grid <- with(observed, interp::interp(x=x1,y=x2,z))
griddf <- subset(data.frame(x = rep(grid$x, nrow(grid$z)),
                            y = rep(grid$y, each = ncol(grid$z)),
                            z = as.numeric(grid$z)),!is.na(z))
p1 <- ggplot(griddf, aes(x, y, z = z)) +
  geom_contour(aes(colour = after_stat(level)),size=2) +
  #geom_point(data = observed,aes(x1,x2)) +
  scale_color_viridis_c()+
  labs(title="Synergistic",color="Prediction",x="x1",y="x2")+
  theme_bw()+ theme(legend.position = "top")
p1
##################################################
#2
set.seed(123)
beta0<- rep(0,200)
beta1<- rep(1,200)
beta2<- rep(1,200)
beta3<- rep(0,200) # c(10,0,10) # antagonism, no interaction, or synergism
x1<- runif(200,min = 0, max = 1)
x2 <- runif(200,min = 0, max = 1)
e <- rnorm(200)

y = beta0 + beta1*x1 + beta2*x2 + beta3*(x1*x2) + e

observed<- tibble(y,x1,x2)
observed

mod <- lm(y~x1*x2,observed)
observed$z <- predict(mod,observed)

grid <- with(observed, interp::interp(x=x1,y=x2,z))
griddf <- subset(data.frame(x = rep(grid$x, nrow(grid$z)),
                            y = rep(grid$y, each = ncol(grid$z)),
                            z = as.numeric(grid$z)),!is.na(z))
p2 <- ggplot(griddf, aes(x, y, z = z)) +
  geom_contour(aes(colour = after_stat(level)),size=2) +
  # geom_point(data = observed,aes(x1,x2)) +
  scale_color_viridis_c()+
  labs(title="Additive",color="Prediction",x="x1",y="x2")+
  theme_bw()+ theme(legend.position = "top")
p2
##################################################
# 3
set.seed(123)
beta0<- rep(0,200)
beta1<- rep(1,200)
beta2<- rep(1,200)
beta3<- rep(-10,200) # c(-10,0,10) # antagonism, no interaction, or synergism
x1<- runif(200,min = 0, max = 1)
x2 <- runif(200,min = 0, max = 1)
e <- rnorm(200)

y = beta0 + beta1*x1 + beta2*x2 + beta3*(x1*x2) + e

observed<- tibble(y,x1,x2)
observed

mod <- lm(y~ x1 * x2 , data = observed)  # rnd effects (1 + x1 | x2)
observed$z <- predict(mod,observed)

grid <- with(observed, interp::interp(x=x1,y=x2,z))
griddf <- subset(data.frame(x = rep(grid$x, nrow(grid$z)),
                            y = rep(grid$y, each = ncol(grid$z)),
                            z = as.numeric(grid$z)),!is.na(z))
p3 <- ggplot(griddf, aes(x, y, z = z)) +
  geom_contour(aes(colour = after_stat(level)),size=2) +
  # geom_point(data = observed,aes(x1,x2)) +
  scale_color_viridis_c()+
  labs(title="Antagonistic",color="Prediction",x="x1",y="x2")+
  theme_bw()+ theme(legend.position = "top")
p3
##################################################
library(patchwork)
p1|p2|p3
##################################################
##################################################


























