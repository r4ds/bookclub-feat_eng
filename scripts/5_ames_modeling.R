library(caret)
library(glmnet)
library(tidymodels)
library(AmesHousing)
library(gridExtra)
library(stringr)
#############################################
# AMES DATA - RMSE VS PENALTY VISUALIZATIONS
#############################################

ames<- ames%>%janitor::clean_names()
ames%>%names%>%sort
set.seed(1109)
split <- initial_split(data = ames,strata = sale_price)
ames_train <- training(split)
ames_test <- testing(split)

ames_folds <- vfold_cv(ames_train,v=5,strata = sale_price)
# additive

additive_rec <-
  recipe(sale_price ~ bldg_type + neighborhood, data = ames_train) %>%
  step_log(sale_price, base = 10) %>%
  #step_BoxCox(lot_area, gr_liv_area, lot_frontage) %>%
  step_other(neighborhood, threshold = 0.05) %>%
  step_dummy(all_nominal()) %>%
  step_interact(~starts_with("bldg_type"):starts_with("neighborhood")) %>%
  step_zv(all_predictors()) %>%
  #step_bs(longitude,latitude, options = list(df = 5)) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())


additive_rec %>%prep()%>%bake(new_data=NULL)


linear_reg_glmnet_spec <-
  linear_reg(penalty = tune(),
             mixture = tune()) %>%
  set_engine('glmnet')

additive_wfl <- workflow()%>%
  add_recipe(additive_rec) %>%
  add_model(linear_reg_glmnet_spec)

control <- control_resamples(save_pred = TRUE,save_workflow = TRUE)
glmn_grid <- expand.grid(penalty = 10^seq(-4, -1, by = 0.1),
                         mixture = seq(.2, 1, by = .2))


doParallel::registerDoParallel()
set.seed(123)
additive_res <-
  tune_grid(
    additive_wfl,
    resamples = ames_folds,
    grid = glmn_grid,
    control = control)

# save(additive_res,file="additive_res.RData")
additive_res <- load("data/additive_res.RData")

additive_res%>%select_best()
#    penalty mixture .config
#    1      0.000316  1 Preprocessor1_Model130

final <- additive_wfl %>%
  finalize_workflow(
    select_by_pct_loss(additive_res, desc(penalty),metric='rmse')
  ) %>%
  last_fit(split)

# save(final,file="final.RData")
final <- load("data/final.RData")

collect_metrics(final)

collect_predictions(final)%>%
  ggplot(aes(sale_price,.pred))+
  geom_point()



#############################################
ames%>%
  ggplot(aes(x = I(year_built*gr_liv_area), y=sale_price))+
  geom_point()+
  geom_smooth()+
  scale_x_log10()+
  scale_y_log10()+
  labs(title="Top selected interaction relationship",
       x = "Year Built * Living Area",
       y="Sale Price")+
  theme_bw()
#############################################
# visualization of the RMSE vs Penalty (two predictors)
collect_metrics(additive_res)%>%
  filter(.metric =="rmse") %>%
  mutate(mixture=as.factor(mixture)) %>%
  ggplot(aes(x=penalty,y=mean,group=mixture,color=mixture)) +
  geom_point()+
  geom_line()+
  scale_x_log10()+
  scale_y_log10()+
  labs(title="Tuning parameter profile",
       x = "Penalty",
       y="RMSE")+
  theme_bw()+
  theme(legend.position = c(0.2,0.5))
####################################################
####################################################
####################################################

# make all pairwise interactions
main_rec <-
  recipe(sale_price ~ bldg_type + neighborhood + year_built +
           gr_liv_area + full_bath + year_sold + lot_area +
           central_air + longitude + latitude + ms_sub_class +
           alley + lot_frontage + pool_area + garage_finish +
           foundation + land_contour + roof_style,
         data = ames_train) %>%
  step_log(sale_price, base = 10) %>%
  step_BoxCox(lot_area, gr_liv_area, lot_frontage) %>%
  step_other(neighborhood, threshold = 0.05) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_bs(longitude,latitude, options = list(df = 5)) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

int_vars <-
  main_rec %>%
  pluck("var_info") %>%
  dplyr::filter(role == "predictor") %>%
  pull(variable)

interactions <- t(combn(as.character(int_vars), 2))
colnames(interactions) <- c("var1", "var2")

interactions <-
  interactions %>%
  as_tibble() %>%
  mutate(
    term =
      paste0(
        "starts_with('",
        var1,
        "'):starts_with('",
        var2,
        "')"
      )
  ) %>%
  pull(term) %>%
  paste(collapse = "+")

interactions <- paste("~", interactions)
interactions <- as.formula(interactions)

main_rec <-
  recipe(sale_price ~ bldg_type + neighborhood + year_built +
           gr_liv_area + full_bath + year_sold + lot_area +
           central_air + longitude + latitude + ms_sub_class +
           alley + lot_frontage + pool_area + garage_finish +
           foundation + land_contour + roof_style,
         data = ames_train) %>%
  step_log(sale_price, base = 10) %>%
  step_BoxCox(lot_area, gr_liv_area, lot_frontage) %>%
  step_other(neighborhood, threshold = 0.05) %>%
  step_dummy(all_nominal()) %>%
  step_interact(interactions) %>%
  step_zv(all_predictors()) %>%
  step_bs(longitude,latitude, options = list(df = 5)) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

main_rec%>%prep()%>%bake(new_data=NULL)
select_by_pct_loss(additive_res, desc(penalty),metric='rmse')

linear_reg_glmnet_spec_best <-
  linear_reg(penalty = tune(),
             mixture = tune()) %>%
  set_engine('glmnet')

additive_wfl_best <- workflow()%>%
  add_recipe(main_rec) %>%
  add_model(linear_reg_glmnet_spec_best)

control <- control_resamples(save_pred = TRUE,save_workflow = TRUE)
glmn_grid <- expand.grid(penalty = 10^seq(-4, -1, by = 0.1),
                         mixture = seq(.2, 1, by = .2))
doParallel::registerDoParallel()
set.seed(123)
main_res <-
  tune_grid(
    additive_wfl_best,
    resamples = ames_folds,
    grid = glmn_grid,
    control = control)

# save(main_res,file="main_res.RData")
main_res <- load("data/main_res.RData")

select_best(main_res,metric = "rmse")
# penalty mixture .config
# <dbl>   <dbl> <fct>
#   1  0.0158     0.2 Preprocessor1_Model023

final_best <- additive_wfl_best %>%
  finalize_workflow(
    select_by_pct_loss(additive_res, desc(penalty),metric='rmse')
  ) %>%
  last_fit(split)

# save(final_best,file="final_best.RData")
final_best <- load("data/final_best.RData")

# this is the visualization in the book!
# visualization of the RMSE vs Penalty (all predictors)
collect_metrics(main_res)%>%
  filter(.metric =="rmse") %>%
  mutate(mixture=as.factor(mixture)) %>%
  ggplot(aes(x=penalty,y=mean,group=mixture,color=mixture)) +
  geom_point()+
  geom_line()+
  scale_x_log10()+
  scale_y_log10()+
  labs(title="Tuning parameter profile",
       x = "Penalty",
       y="RMSE")+
  theme_bw()+
  theme(legend.position = c(0.2,0.5))


####################################################
####################################################
####################################################

