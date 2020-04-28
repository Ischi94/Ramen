library(tidyverse)
library(tidymodels)

# download data
ramen_ratings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

# calculate bayesian average taking the number of ratings into account
# we're using the average rating for all ramen as a reference prior
# m = mean of all rating
m <- ramen_ratings %>% 
  summarise(mean_rating = mean(stars, na.rm = TRUE)) %>% 
  pull(mean_rating) %>% 
  round(., 2)

# C represents how confident we in our prior. 
# It is equivalent to a number of observations. I set it to 3 in our case. 
C = 3

# calculate bayesian averages per brand
ramen_bayes_av <- ramen_ratings %>% 
  # average rating per brand
  group_by(brand, country, style) %>% 
  summarise(mean_rating = mean(stars, na.rm = TRUE), n = n()) %>% 
  ungroup() %>% 
  mutate(bayes_average = (C * m + mean_rating * n)/ (C + n)) %>% 
  select(-mean_rating, -n) %>% 
  # remove missing values
  drop_na()


ramen_bayes_av %>% 
  count(country) %>% View()

