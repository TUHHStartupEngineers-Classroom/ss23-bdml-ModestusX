# Challenge 1 - BDML - Fundamentals of Machine Learning
## Author: Moritz Henkel

# 01 Libraries
library(tidyverse)
library(tidyquant)
library(broom)
library(umap)
library(plotly)
library(ggrepel)

# 02 Load DATA
sp_500_prices_tbl <- read_rds("Fundamentals of Machine Learning/Business Decisions with Machine Learning/sp_500_prices_tbl.rds")
sp_500_index_tbl <- read_rds("Fundamentals of Machine Learning/Business Decisions with Machine Learning/sp_500_index_tbl.rds")                         

# 03 Question 1: Which stock prices behave similarly?
## 03 Step 1 - Convert stock prices to a standardized format
sp_500_daily_returns_tbl <- sp_500_prices_tbl %>%
  select(symbol,date,adjusted) %>%
  filter(date > '2018-01-01') %>%
  group_by(symbol) %>%
  mutate(price_lagged = lag(adjusted, n=1)) %>%
  ungroup() %>%
  na.omit() %>%
  mutate(pct_return = (adjusted-price_lagged)/price_lagged) %>%
  select(symbol,date,pct_return)

## 03 Step 2 - Convert to User-Item Format
stock_date_matrix_tbl <- sp_500_daily_returns_tbl %>%
  pivot_wider(names_from = date, values_from = pct_return, values_fill = 0)

## 03 Step 3 - Perform K-Means Clustering
kmeans_obj <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  kmeans(centers = 4, nstart = 20)

## 03 Step 4 - Find the optimal value of K
kmeans_mapper <- function(center = 3) {
  stock_date_matrix_tbl %>%
    select(-symbol) %>%
    kmeans(centers = center, nstart = 20)
}

k_means_mapped_tbl <- tibble(centers = 1:30) %>%
  mutate(k_means = centers %>% map(kmeans_mapper)) %>%
  mutate(glance  = k_means %>% map(glance))

#The glance column contains tibbles that can be unnested. 
#The final data can be used for the scree plot.
k_means_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%

#Visualization of Scree Plot
ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2DC6D6", size = 4) +
  geom_line(color = "#2DC6D6", size = 1) +
  # Add labels (which are repelled a little)
  ggrepel::geom_label_repel(aes(label = centers), color = "#2DC6D6") + 
  
  # Formatting
  labs(title = "Skree Plot",
       subtitle = "Measures the distance each of the stocks are from the closes K-Means center",
       caption = "Conclusion: Based on the Scree Plot, we select 5 clusters to segment the stocks.")

## 03 Step 5 - Apply UMAP
#Apply UMAP
umap_results <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  umap()

#Convert umap results to tibble with symbols
umap_results_tbl <- umap_results$layout %>%
  as_tibble() %>%
  bind_cols(stock_date_matrix_tbl %>% select(symbol))

# Visualize UMAP results
umap_results_tbl %>%
ggplot(aes(V1,V2)) +
  geom_point(color = "#2DC6D6", size = 2, alpha = 0.5) + 
  
  # Formatting
  theme_tq() +
  labs(title = "UMAP Projection")

## 03 Step 6 - Combine K-Means and UMAP
# Get the k_means_obj from the 10th center
# Store as k_means_obj
k_means_obj <- k_means_mapped_tbl %>%
  pull(k_means) %>%
  pluck(10)

# Use your dplyr & broom skills to combine the k_means_obj with the umap_results_tbl
# Output: umap_kmeans_results_tbl
umap_kmeans_results_tbl <- k_means_obj %>%
  augment(stock_date_matrix_tbl) %>%
  # Select the data we need
  select(symbol, .cluster) %>%
  left_join(umap_results_tbl, by = "symbol") %>%
  left_join(sp_500_index_tbl %>% select(symbol, company, sector), by = "symbol") %>%
  mutate(label_text = str_glue("Ticker: {symbol}"))

# Visualize the combined K-Means and UMAP results
umap_kmeans_results_tbl %>%
  #Visualize the K-Means and UMAp results
  ggplot(aes(V1,V2,color = .cluster))+
  geom_point(alpha = 0.5)+
  geom_label_repel(aes(label = label_text), size = 2, fill = "#282A36") +
  scale_color_manual(values=c("black", "violet", "green","red","orange","purple","pink","blue","brown","yellow"))

















