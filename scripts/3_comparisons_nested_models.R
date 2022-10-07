#####

library(AmesHousing)
ames<-make_ames()

library(tidymodels)
set.seed(1111)
split<-initial_split(ames,strata = Sale_Price)
training<-training(split)

# training%>%names

# example of not statistically signicant interaction
mod0 <- lm(Sale_Price ~ Year_Sold + Lot_Area, training)
summary(mod0)
mod00 <- lm(Sale_Price ~ Year_Sold * Lot_Area, training)
summary(mod00)
anova(mod0,mod00)

# example of statistically signicant interaction
mod1 <- lm(Sale_Price ~ Year_Built + Gr_Liv_Area, training)
summary(mod1)
mod2 <- lm(Sale_Price ~ Year_Built * Gr_Liv_Area, training)
summary(mod2)
anova(mod1,mod2)


