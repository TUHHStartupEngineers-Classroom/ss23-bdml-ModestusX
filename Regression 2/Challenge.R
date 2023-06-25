### Regression 2 Challenge [Preprocessing/Recipe] -----------------------------------
## 01 -Libraries-
library(tidyverse)
library(parsnip)
library(recipes)
library(rsample)
library(yardstick)
library(rpart.plot)
library(tidymodels)
library(skimr)
library(dplyr)
## 02 -Building the model-
# Modeling sales ----------------------------------------------------------------
bike_orderlines_tbl <- readRDS("Fundamentals of Machine Learning/Business Decisions with Machine Learning/bike_orderlines.rds")
glimpse(bike_orderlines_tbl)

model_sales_tbl <- bike_orderlines_tbl %>%
  select(total_price, model, category_2, frame_material) %>%
  
  group_by(model, category_2, frame_material) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  arrange(desc(total_sales))

model_sales_tbl %>%
  mutate(category_2 = as_factor(category_2) %>% 
           fct_reorder(total_sales, .fun = max) %>% 
           fct_rev()) 
# Basis for Recipe/Features ----------------------------------------------------------------
bike_features_tbl <- readRDS("Fundamentals of Machine Learning/Business Decisions with Machine Learning/bike_features_tbl.rds") %>%
  unnest(`Brake Rotor`) %>%
  na.omit() %>% 
  select(-Brake) %>%
  mutate_if(is.character, as.factor)

glimpse(bike_features_tbl)

bike_features_tbl %>% distinct(category_2)

# run both following commands at the same time
set.seed(seed = 1113)
split_obj <- rsample::initial_split(bike_features_tbl, prop   = 0.80, 
                                    strata = "category_2")

# Check if testing contains all category_2 values
split_obj %>% training() %>% distinct(category_2)
split_obj %>% testing() %>% distinct(category_2)

# Assign training and test data
train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)

# We have to remove spaces and dashes from the column names
train_tbl <- train_tbl %>% set_names(str_replace_all(names(train_tbl), " |-", "_"))
test_tbl  <- test_tbl  %>% set_names(str_replace_all(names(test_tbl),  " |-", "_"))

## 03 -Create features with a recipe-
# Modeling Recipe for features [Groupsets] ----------------------------------------------------------------

bike_recipe <- recipe(price ~ ., data = train_tbl) %>% 
  step_rm(category_1,category_3,url,model_year) %>% 
  step_dummy(all_nominal(), - all_outcomes(),one_hot = TRUE) %>% 
  prep()





####Remember, you don’t need to set the flags by yourself (see all_nominal()) --- you will probably only need bake() or prep().
?recipe
?step_dummy
?prep
?bake

recipe_obj <- recipe(...) %>% 
  step_rm(...) %>% 
  step_dummy(... ) %>% # Check out the argument one_hot = T
  prep()

train_transformed_tbl <- bake(..., ...)
test_transformed_tbl  <- bake(..., ...)

## 04 -Bundle the model and recipe with the workflow package-


## 05 -Evaluate your model with the yardstick package-


#dont know about this one
train_transformed_tbl <- bake(bike_recipe, new_data = train_tbl)
test_transformed_tbl <- bake(bike_recipe, new_data = test_tbl)
