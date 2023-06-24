# Load data
library(tidyverse)
library(readxl)
library(dplyr)

employee_attrition_tbl <- read_csv("AutoML 1/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl    <- read_excel("Fundamentals of Machine Learning/Business Decisions with Machine Learning/data_definitions.xlsx", sheet = 1, col_names = FALSE)
View(definitions_raw_tbl)

employee_attrition_tbl %>% 
  ggplot(aes(Education)) +
  geom_bar()

# Data preparation ----
# Human readable

definitions_tbl <- definitions_raw_tbl %>% 
  fill(...1, .direction = "down") %>%
  filter(!is.na(...2)) %>%
  separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
  rename(column_name = ...1) %>%
  mutate(key = as.numeric(key)) %>%
  mutate(value = value %>% str_replace(pattern = "'", replacement = "")) 
definitions_tbl

# DATA PREPARATION ----
# Human readable ----
definitions_list <- definitions_tbl %>% 
  
  # Mapping over lists
  
  # Split into multiple tibbles
  split(.$column_name) %>%
  # Remove column_name
  map(~ select(., -column_name)) %>%
  # Convert to factors because they are ordered an we want to maintain that order
  map(~ mutate(., value = as_factor(value))) 

# definitions_list[[1]]
definitions_list[["Education"]]

# Rename columns
for (i in seq_along(definitions_list)) {
  list_name <- names(definitions_list)[i]
  colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
}

definitions_list[["Education"]]

data_merged_tbl <- list(HR_Data = employee_attrition_tbl) %>%
  
  # Join everything
  append(definitions_list, after = 1) %>%
  reduce(left_join) %>%
  
  # Remove unnecessary columns
  select(-one_of(names(definitions_list))) %>%
  
  # Format the "_value"
  set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>%
  
  # Resort
  select(sort(names(.))) 

# Return only unique values of BusinessTravel
data_merged_tbl %>% 
  distinct(BusinessTravel)

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  glimpse()

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  select_if(is.factor) %>%
  glimpse()

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  select_if(is.factor) %>%
  map(levels)

data_processed_tbl <- data_merged_tbl %>%        
  mutate_if(is.character, as.factor) %>%
  mutate(
    BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
                                                    "Travel_Rarely", 
                                                    "Travel_Frequently"),
    MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
                                                   "Married", 
                                                   "Divorced")
  )

data_processed_tbl %>% 
  select_if(is.factor) %>% 
  map(levels)

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
process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl) %>% 
  glimpse()

# DATA PREPARATION ----
# Machine readable ----

# libraries
library(rsample)
library(recipes)

# Processing pipeline
# If we had stored our script in an external file
#source("00_scripts/data_processing_pipeline.R")

# If we had our raw data already split into train and test data
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_redable_tbl   <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)

# Split into test and train
set.seed(seed = 1113)
split_obj <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)

# Assign training and test data
train_readable_tbl <- training(split_obj)
test_readable_tbl  <- testing(split_obj)

# Plot Faceted Histgoram function

# To create a function and test it, we can assign our data temporarily to data
data <- train_readable_tbl 

plot_hist_facet <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, fill = "#2dc6d6", color = "white", 
                            ncol = 5, scale = "free") {
  
  data_factored <- data %>%
    
    # Convert input to make the function fail safe 
    # (if other content might be provided)
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    
    # Data must be in long format to make facets
    pivot_longer(cols = everything(),
                 names_to = "key",
                 values_to = "value",
                 # set key = factor() to keep the order
                 names_transform = list(key = forcats::fct_inorder)) 
  
  if (fct_reorder) {
    data_factored <- data_factored %>%
      mutate(key = as.character(key) %>% as.factor())
  }
  
  if (fct_rev) {
    data_factored <- data_factored %>%
      mutate(key = fct_rev(key))
  }
  
  g <- data_factored %>%
    ggplot(aes(x = value, group = key)) +
    geom_histogram(bins = bins, fill = fill, color = color) +
    facet_wrap(~ key, ncol = ncol, scale = scale)
  
  return(g)
  
}

# Example calls
train_readable_tbl %>% plot_hist_facet()
train_readable_tbl %>% plot_hist_facet(fct_rev = T)

# Bring attirtion to the top (alt.: select(Attrition, everything()))
train_readable_tbl %>% 
  relocate(Attrition) %>% 
  plot_hist_facet()

# Data Preprocessing With Recipes ----

# Plan: Correlation Analysis

# 1. Zero Variance Features ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors())

recipe_obj %>% 
  prep()

# 2. Transformations ---- (for skewed features)
library(PerformanceAnalytics)  # for skewness  

skewed_feature_names <- train_readable_tbl %>%
  select(where(is.numeric)) %>%
  map_df(skewness) %>%
  pivot_longer(cols = everything(),
               names_to = "key",
               values_to = "value",
               names_transform = list(key = forcats::fct_inorder)) %>%
  arrange(desc(value)) %>%
  
  # Let's set the cutoff value to 0.7 (beccause TrainingTimesLastYear does not seem to be that skewed)
  filter(value >= 0.7) %>%
  pull(key) %>%
  as.character()

train_readable_tbl %>%
  select(all_of(skewed_feature_names)) %>%
  plot_hist_facet()

!skewed_feature_names %in% c("JobLevel", "StockOptionLevel")

skewed_feature_names <- train_readable_tbl %>%
  select(where(is.numeric)) %>%
  map_df(skewness) %>%
  pivot_longer(cols = everything(),
               names_to = "key",
               values_to = "value",
               names_transform = list(key = forcats::fct_inorder)) %>%
  arrange(desc(value)) %>%
  filter(value >= 0.7) %>%
  filter(!key %in% c("JobLevel", "StockOptionLevel")) %>%
  pull(key) %>%
  as.character()

# We need to convert those columns to factors in the next step
factor_names <- c("JobLevel", "StockOptionLevel")

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor)

recipe_obj %>% 
  prep() %>% 
  bake(train_readable_tbl) %>% 
  select(skewed_feature_names) %>%
  plot_hist_facet() 

# 3. Center and scale

# Plot numeric data
train_readable_tbl %>% 
  select(where(is.numeric)) %>% 
  plot_hist_facet()

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

# You can compare the means attribute before and after prepping the recipe
recipe_obj$steps[[4]] # before prep
prepared_recipe <- recipe_obj %>% prep()
prepared_recipe$steps[[4]]

prepared_recipe %>%
  bake(new_data = train_readable_tbl) %>%
  select(where(is.numeric)) %>% 
  plot_hist_facet()

# 4. Dummy variables ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal()) %>% 
  
  # prepare the final recipe
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)

train_tbl %>% glimpse()

test_tbl <- bake(recipe_obj, new_data = test_readable_tbl)

#Correlation analysis
?stats::cor

train_tbl %>%
  
  # Convert characters & factors to numeric
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  
  # Correlation
  cor(use = "pairwise.complete.obs") %>% 
  as_tibble() %>%
  mutate(feature = names(.)) %>% 
  select(feature, Attrition_Yes) %>% 
  
  # Filter the target, because we now the correlation is 100%
  filter(!(feature == "Attrition_Yes")) %>% 
  
  # Convert character back to factors
  mutate(across(where(is.character), as_factor))

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE)
 
  
library(dplyr)
library(ggplot2) 
#error here

data_cor <- train_tbl %>%
  # Correlation
  get_cor(target = Attrition_Yes, fct_reorder = TRUE, fct_rev = TRUE)()

data_cor <- data_cor %>%
  # Create label text
  mutate(feature_name_text = round(Attrition_Yes, digits = 2)) %>%
  
  # Create flags so that we can change the color for positive and negative
  mutate(Correlation = ifelse(Attrition_Yes >= 0, "Positive", "Negative") %>% as.factor())

data_cor %>%
  ggplot(aes(x = Attrition_Yes, y = feature, group = feature)) +
  geom_point(aes(color = Correlation), size = 2) +
  geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = 1) +
  geom_vline(xintercept = 0, color = "black", size = 0.5) +
  expand_limits(x = c(-1, 1)) +
  scale_color_manual(values = c("red", "#2dc6d6")) +
  geom_label(aes(label = feature_name_text), hjust = "outward")









#   2. Employment features: department, job role, job level
train_tbl %>%
  select(Attrition_Yes, contains("employee"), contains("department"), contains("job")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F) 

#   3. Compensation features: HourlyRate, MonthlyIncome, StockOptionLevel 
train_tbl %>%
  select(Attrition_Yes, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)

#   4. Survey Results: Satisfaction level, WorkLifeBalance 
train_tbl %>%
  select(Attrition_Yes, contains("satisfaction"), contains("life")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)

#   5. Performance Data: Job Involvment, Performance Rating
train_tbl %>%
  select(Attrition_Yes, contains("performance"), contains("involvement")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)

#   6. Work-Life Features 
train_tbl %>%
  select(Attrition_Yes, contains("overtime"), contains("travel")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)

#   7. Training and Education 
train_tbl %>%
  select(Attrition_Yes, contains("training"), contains("education")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)

#   8. Time-Based Features: Years at company, years in current role
train_tbl %>%
  select(Attrition_Yes, contains("years")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = F, fct_rev = F)
