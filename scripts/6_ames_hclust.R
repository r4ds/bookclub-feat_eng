library(AmesHousing)

# hclust
ames <- make_ames() %>%
  janitor::clean_names()


# library(corrplot)
# numeric_ames <- ames%>%
#   select_if(is.numeric)
#
# cor.matrix_ames_num <- cor(as.matrix(numeric_ames))
# quartz()
# corrplot(cor.matrix_ames_num)
#
# clust_ames <- hclust(dist(cor.matrix_ames_num),"ave")
# quartz()
# plot(clust_ames)


# predictors selection
library(corrplot)
numeric_ames <- ames%>%
  select_if(is.numeric)

numeric_ames%>%names

numeric_ames_short <- recipe(sale_price~., data = numeric_ames) %>%
  step_corr(all_numeric_predictors(),threshold = 0.7)%>%
  prep()%>%
  bake(new_data=NULL)

cor.matrix_ames_num_sh <- cor(as.matrix(numeric_ames_short))
quartz()
corrplot(cor.matrix_ames_num_sh)

clust_ames_sh <- hclust(dist(cor.matrix_ames_num_sh),"ave")
quartz()
plot(clust_ames_sh)
