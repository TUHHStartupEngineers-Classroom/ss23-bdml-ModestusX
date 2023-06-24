## Challenge AutoML2
## Author: Moritz Henkel

# H2O modeling
library(h2o)
library(tidyverse)
library(readxl)
library(tidymodels)
#Process HR Data Pipeline
process_hr_data_readable <- function(data, definitions_tbl) {
  
  definitions_list <- definitions_tbl %>%
    fill(...1, .direction = "down") %>%
    filter(!is.na(...2)) %>%
    separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
    rename(column_name = ...1) %>%
    mutate(key = as.numeric(key)) %>%
    mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>%
    split(.$column_name) %>%
    map(~ select(., -column_name)) %>%
    map(~ mutate(., value = as_factor(value))) 
  
  for (i in seq_along(definitions_list)) {
    list_name <- names(definitions_list)[i]
    colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
  }
  
  data_merged_tbl <- list(HR_Data = data) %>%
    append(definitions_list, after = 1) %>%
    reduce(left_join) %>%
    select(-one_of(names(definitions_list))) %>%
    set_names(str_replace_all(names(.), pattern = "_value", 
                              replacement = "")) %>%
    select(sort(names(.))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
                                                      "Travel_Rarely", 
                                                      "Travel_Frequently"),
      MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
                                                     "Married", 
                                                     "Divorced")
    )
  
  return(data_merged_tbl)
  
}
#Modeling
employee_attrition_tbl          <- read_csv("AutoML 1/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl             <- read_excel("Fundamentals of Machine Learning/Business Decisions with Machine Learning/data_definitions.xlsx", sheet = 1, col_names = FALSE)
employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)
set.seed(seed = 1113)
split_obj                       <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)
train_readable_tbl              <- training(split_obj)
test_readable_tbl               <- testing(split_obj)

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# Modeling
h2o.init()

# Split data into a training and a validation data frame
# Setting the seed is just for reproducability
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

# Set the target and predictors
y <- "Attrition"
x <- setdiff(names(train_h2o), y)

?h2o.automl

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)

typeof(automl_models_h2o)
## "S4"

slotNames(automl_models_h2o)
## [1] "project_name"   "leader"         "leaderboard"    "event_log"      "modeling_steps" "training_info" 

automl_models_h2o@leaderboard
##                                              model_id       auc   logloss     aucpr mean_per_class_error      rmse        mse
## 1 StackedEnsemble_BestOfFamily_AutoML_20200820_190823 0.8585439 0.2992854 0.5869929            0.2406915 0.2978416 0.08870964
## 2          GBM_grid__1_AutoML_20200820_190823_model_3 0.8494016 0.3137896 0.5165541            0.2386968 0.3098134 0.09598435
## 3 DeepLearning_grid__1_AutoML_20200820_190823_model_1 0.8479056 0.3066365 0.6154288            0.2583112 0.3071528 0.09434283
## 4      XGBoost_grid__1_AutoML_20200820_190823_model_5 0.8439162 0.3057109 0.5299331            0.2061170 0.3071419 0.09433613
## 5    StackedEnsemble_AllModels_AutoML_20200820_190823 0.8425864 0.3211612 0.5205591            0.2539894 0.3107399 0.09655928
## 6      XGBoost_grid__1_AutoML_20200820_190823_model_6 0.8257979 0.3211936 0.5009608            0.2536569 0.3111129 0.09679122
##
## [30 rows x 7 columns] 

automl_models_h2o@leader

# Depending on the algorithm, the output will be different
h2o.getModel("DeepLearning_grid__1_AutoML_20200820_190823_model_1")

# Extracts and H2O model name by a position so can more easily use h2o.getModel()
extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = T) {
  
  model_name <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(n) %>%
    pull(model_id)
  
  if (verbose) message(model_name)
  
  return(model_name)
  
}

automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(6) %>% 
  h2o.getModel()

h2o.getModel("DeepLearning_grid__1_AutoML_20200820_190823_model_1") %>% 
  h2o.saveModel(path = "04_Modeling/h20_models/")

h2o.loadModel("04_Modeling/h20_models/DeepLearning_grid__1_AutoML_20200820_190823_model_1")

# Choose whatever model you want
stacked_ensemble_h2o <- h2o.loadModel("04_Modeling/h20_models/StackedEnsemble_BestOfFamily_AutoML_20200820_190823")
stacked_ensemble_h2o

predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(predictions)
## [1] "environment"

predictions_tbl <- predictions %>% as_tibble()