# Ischemic Stroke decision tree
library(tidymodels)
library(rpart)
library(rpart.plot)

load(url("https://github.com/topepo/FES/blob/master/Data_Sets/Ischemic_Stroke/stroke_data.RData?raw=true"))

all_stroke <- rbind(stroke_test,stroke_train)
set.seed(2222)
split_is <- initial_split(all_stroke)
training_is <- training(split_is)
test_is <- testing(split_is)

class_tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")


selected_train <- 
  training_is %>%
  dplyr::select(one_of(VC_preds), Stroke)

is_recipe <- recipe(Stroke ~ ., data = selected_train) %>%
  #step_interact(int_form)  %>%
  step_corr(all_predictors(), threshold = 0.75) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  step_zv(all_predictors())

is_recipe%>%prep()%>%bake(new_data=NULL)

is_wfl <- workflow() %>%
  add_model(class_tree_spec) %>%
  add_recipe(is_recipe)

is_dt_fit_wfl <- is_wfl %>%
  fit(data = selected_train,
      control = control_workflow())

is_dt_fit_wfl%>%
  extract_fit_engine() %>%
  rpart.plot::rpart.plot(roundint = FALSE)



