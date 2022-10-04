# ames: a practical approach for predictors selection

library(caret)
library(glmnet)
library(tidymodels)
library(AmesHousing)

# ------------------------------------------------------------------------------

ames <- make_ames()

set.seed(955)
ames_split <- initial_split(ames)
ames_train <- training(ames_split)

set.seed(24873)
ames_folds <- vfold_cv(ames_train)

ames_ind <- rsample2caret(ames_folds)

main_rec <-
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built +
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude + MS_SubClass +
           Alley + Lot_Frontage + Pool_Area + Garage_Finish +
           Foundation + Land_Contour + Roof_Style,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area, Lot_Frontage) %>%
  step_other(Neighborhood, threshold = 0.05) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_bs(Longitude, Latitude, options = list(df = 5))

load(url("https://github.com/topepo/FES/blob/master/07_Detecting_Interaction_Effects/7_05_Approaches_when_Complete_Enumeration_is_Practically_Impossible/ames_h_stats.RData?raw=true"))
#### H STATISTICS ######  ###### ######  ############  ############  ######
int_vars <-
  h_stats %>%
  dplyr::filter(Estimator == "Bootstrap" & H > 0.001) %>%
  pull(Predictor)

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

int_rec <-
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built +
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude + MS_SubClass +
           Alley + Lot_Frontage + Pool_Area + Garage_Finish +
           Foundation + Land_Contour + Roof_Style,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area, Lot_Frontage) %>%
  step_other(Neighborhood, threshold = 0.05) %>%
  step_dummy(all_nominal()) %>%
  # interaction
  step_interact(interactions) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_bs(Longitude, Latitude, options = list(df = 5))

# ------------------------------------------------------------------------------

ctrl <-
  trainControl(
    method = "cv",
    index = ames_ind$index,
    indexOut = ames_ind$indexOut
  )

glmn_grid <- expand.grid(alpha = seq(.2, 1, by = .2),
                         lambda = 10^seq(-4, -1, by = 0.1))

main_glmn_h <-
  train(main_rec,
        data = ames_train,
        method = "glmnet",
        tuneGrid = glmn_grid,
        trControl = ctrl
  )

int_glmn_h <-
  train(int_rec,
        data = ames_train,
        method = "glmnet",
        tuneGrid = glmn_grid,
        trControl = ctrl
  )

p <-
  ggplot(int_glmn_h) +
  scale_x_log10() +
  theme_bw() +
  theme(legend.position = "top")

p
# ------------------------------------------------------------------------------

main_info_h <-
  list(
    all =
      main_rec %>%
      prep(ames_train) %>%
      juice(all_predictors()) %>%
      ncol(),
    main = length(predictors(main_glmn_h)),
    perf = getTrainPerf(main_glmn_h)
  )

int_info_h <-
  list(
    all =
      int_rec %>%
      prep(ames_train) %>%
      juice(all_predictors()) %>%
      ncol(),
    main = sum(!grepl("_x_", predictors(int_glmn_h))),
    int = sum(grepl("_x_", predictors(int_glmn_h))),
    perf = getTrainPerf(int_glmn_h)
  )

# save(main_info_h, int_info_h, int_glmn_h, file = "ames_glmnet_h.RData")

int_glmn_h$bestTune
int_info_h$main
int_info_h$int

############################################################

library(tidymodels)
library(AmesHousing)
library(furrr)
library(stringr)
library(cli)
library(crayon)
library(stringr)

source(url("https://raw.githubusercontent.com/topepo/FES/master/07_Detecting_Interaction_Effects/7_05_Approaches_when_Complete_Enumeration_is_Practically_Impossible/fsa_functions.R"))
source(url("https://raw.githubusercontent.com/topepo/FES/master/07_Detecting_Interaction_Effects/clean_value.R"))


# ------------------------------------------------------------------------------

ames <- make_ames()

set.seed(955)
ames_split <- initial_split(ames)
ames_train <- training(ames_split)

set.seed(24873)
ames_folds <- vfold_cv(ames_train)

# ------------------------------------------------------------------------------

ames_rec <-
  recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built +
           Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
           Central_Air + Longitude + Latitude + MS_SubClass +
           Alley + Lot_Frontage + Pool_Area + Garage_Finish +
           Foundation + Land_Contour + Roof_Style,
         data = ames_train) %>%
  step_log(Sale_Price, base = 10) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area, Lot_Frontage) %>%
  step_other(Neighborhood, threshold = 0.05) %>%
  step_dummy(all_nominal()) %>%
  step_bs(Longitude, Latitude, options = list(df = 5))  %>%
  step_zv(all_predictors())

# ------------------------------------------------------------------------------

multi_metric <- metric_set(rmse, mae, rsq)

lr_spec <- linear_reg() %>% set_engine("lm")

set.seed(236)
# doesn't work
ames_search <- fsa_two_way(ames_folds, ames_rec, lr_spec, multi_metric)

# for parallel processing
# plan(multiprocess) deprecated
plan(multisession)

# https://bookdown.org/max/FES/approaches-when-complete-enumeration-is-practically-impossible.html#tab:interactions-fsa-ames-results
ames_search %>%
  arrange(perf) %>%
  dplyr::filter(perf < ames_search %>% slice(1) %>% pull(perf)) %>%
  dplyr::select(-iter, -seeds, -change, -swaps, RMSE = perf) %>%
  distinct() %>%
  mutate(
    var_1 = clean_value(var_1),
    var_2 = clean_value(var_2)
  ) %>%
  group_by(var_1, var_2) %>%
  summarize(
    pval = pval[which.min(RMSE)],
    RMSE = RMSE[which.min(RMSE)]
  ) %>%
  ungroup() %>%
  arrange(RMSE)











