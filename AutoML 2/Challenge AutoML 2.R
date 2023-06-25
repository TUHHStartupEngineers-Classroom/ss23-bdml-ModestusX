## Challenge AutoML2
## Challenge description: Goal is to predict whether or not
## a product will be put on backorder status.
## given a number of product metrics such as:
## current inventory, transit time, demand forecasts and prior sales. 
## It’s a classic Binary Classification problem.

## -01 Libraries-
library(h2o)
library(tidyverse)
library(readxl)
library(rsample)
library(recipes)
library(tidymodels)
## -02 Load the training & test dataset-
#Import and clean Data
product_backorders_tbl_raw <- read_csv("AutoML 2/product_backorders.csv")
product_backorders_tbl %>% glimpse
product_backorders_tbl %>% class
product_backorders_tbl <- product_backorders_tbl_raw %>%
  replace(is.na(.), 0) %>%
  select(national_inv,forecast_3_month,forecast_6_month,forecast_9_month,sales_1_month,sales_3_month,sales_9_month,went_on_backorder,lead_time)

#Split the Dataset---
set.seed(seed = 1113)
split_obj                       <- rsample::initial_split(product_backorders_tbl, prop = 0.85)
train_readable_tbl              <- training(split_obj)
test_readable_tbl               <- testing(split_obj)

train_readable_tbl %>% glimpse

#Add recipe
recipe_obj <- recipe(went_on_backorder ~., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_dummy(all_nominal()) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

train_tbl %>% glimpse
# -03 Specify the response and predictor variables-
#Modeling
h2o.init()

#Split data into a training and a validation data frame
#Setting the seed is just for reproducability
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)
#Set the target and predictors
y <- "went_on_backorder_Yes"
x <- setdiff(names(train_h2o), y)
# -04 Run AutoML specifying the stopping criterion-
#Computation
automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)
# -05 View the leaderboard-
typeof(automl_models_h2o)

slotNames(automl_models_h2o)

automl_models_h2o@leaderboard

automl_models_h2o@leader
# -06 Predicting using Leader Model-
stacked_ensemble_h2o <- h2o.loadModel("AutoML 2/Models/StackedEnsemble_AllModels_1_AutoML_2_20230621_163345")


#Make predictions
predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))
typeof(predictions)
predictions_tbl <- predictions %>% as_tibble()
# -07 Save the leader model-
h2o.getModel("StackedEnsemble_AllModels_1_AutoML_2_20230621_163345") %>% 
  h2o.saveModel(path = "AutoML 2/Models/")


