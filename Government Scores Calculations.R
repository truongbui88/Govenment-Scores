library(tidyverse)
library(rio)
library(janitor)


#Import and clean data
score_data <- import("Value of Government Scores.xlsx") %>% 
  select(1:8) %>% 
  clean_names() %>% 
  rename(high_grad_rate = high_school_graduation_rate_calculated_from_nces_data,
         share_of_homeless = share_of_sheltered_homeless_persons_from_hud,
         water_quality= water_quality_scores_from_lawn_starter)


#Calculate z-scores and rankings
score_data_rank <- score_data %>% 
  mutate(across(high_grad_rate:water_quality, ~ (.x - mean(.x))/sd(.x), .names = "{.col}_zscore")) %>% 
  mutate(across(violent_crime_rate_zscore:property_crime_rate_zscore, ~ -.x)) %>%    #negate these scores since lower crime rates are better than higher rates
  rowwise() %>% 
  mutate(avg_zscore = mean(c_across(high_grad_rate_zscore:water_quality_zscore))) %>% 
  ungroup() %>% 
  mutate(across(c(high_grad_rate_zscore:avg_zscore), ~ (.x - min(.x))/(max(.x) - min(.x)), .names = "{.col}_norm"),
         across(c(high_grad_rate_zscore:avg_zscore), ~ min_rank(desc(.x)), .names = "{.col}_rank"))


export(score_data_rank, "score_data.xlsx")
