library(tidyverse)
library(manipulate)
##################################################
  mod <- lm(y~x1*x2,observed)
  plot <- ggplot(observed,aes(x2,y))+
    geom_point()+
    geom_smooth(method="lm")+
    theme_bw()

# We can use the function `manipulate()` from {manipulate} package, to assess the level of the slope to identify the model line:

  manipulate(plot +
               geom_abline(intercept=intercept,
                           slope=slope),
             intercept=slider(min=-2,max=1,step=0.1),
             slope=slider(min=-9,max=1,step=0.1))



