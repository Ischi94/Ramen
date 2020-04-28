library(tidyverse)
library(tidymodels)
library(ggimage)

# download data
ramen_ratings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")


# Bayesian average rating -------------------------------------------------


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
  mutate(bayes_average = (C * m + mean_rating * n) / (C + n)) %>%
  select(-mean_rating,-n) %>%
  # remove missing values
  drop_na()



# Data processing ---------------------------------------------------------


# lumping countries to continents may facilitate interpretation
ramen_good <- ramen_bayes_av %>%
  # clean some countrynames
  mutate(
    country = recode(
      country,
      Dubai = "United Arab Emirates",
      Holland = "Netherlands",
      Phlippines = "Philippines",
      Sarawak = "Malaysia"
    )
  ) %>%
  # lump countries to continents
  mutate(
    continent = countrycode::countrycode(
      sourcevar = .$country,
      origin = "country.name",
      destination = "continent"
    )
  ) %>%
  select(-country) %>% 
  # lump brands 
  mutate(brand = fct_lump(brand, 40)) %>%
  replace_na(list(style = "Other")) %>%
  mutate(brand = fct_relevel(brand, "Other")) %>% 
  # I am interested in what makes a ramen a good ramen. For this, we don't need all 
  # the rating data. Instead, we can split the data in better than average and worse
  # we previously defined m as the average
  mutate(bayes_average = if_else(bayes_average >= m, "Good", "Bad"), 
         bayes_average = factor(bayes_average, levels = c("Good", "Bad")))



# Train and test split ----------------------------------------------------

set.seed(seed = 123) 

# split it up using the outcome for stratified sampling
tt_split <- ramen_good %>% 
  initial_split(prop = 0.8, strata = "bayes_average")
  
# assign it to train and test
train <- tt_split %>% training() 
test  <- tt_split %>% testing()


# Recipe ------------------------------------------------------------------

recipe_glm <- recipe(bayes_average ~ ., data = train) %>%
  step_string2factor(all_nominal(), -all_outcomes()) 

# prep it
recipe_prepped <- recipe_glm %>% prep(train)

# bake it
train_baked <- bake(recipe_prepped, new_data = train)
test_baked  <- bake(recipe_prepped, new_data = test)


# Model fitting logistic reg ----------------------------------------------

logistic_glm <-
  logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(bayes_average ~ ., data = train_baked)


# make predictions
predictions_glm <- logistic_glm %>%
  predict(new_data = test_baked) %>%
  bind_cols(test_baked %>% select(bayes_average))

# confusion matrix
predictions_glm %>%
  conf_mat(bayes_average, .pred_class) 

# accuracy
glm_accuracy <- predictions_glm %>%
  metrics(bayes_average, .pred_class) %>%
  select(-.estimator) %>%
  filter(.metric == "accuracy") %>% 
  pull() %>% round(2)

# precision 
glm_precision <- precision(predictions_glm, bayes_average, .pred_class) %>%
  select(.estimate) %>% 
  pull() %>% round(2)

# recall 
glm_recall <- recall(predictions_glm, bayes_average, .pred_class) %>%
  select(.estimate) %>% 
  pull() %>% round(2)

# F1 score
glm_f1score <- predictions_glm %>%
  f_meas(bayes_average, .pred_class) %>%
  select(-.estimator) %>% 
  pull() %>% round(2)


# make data frame
mdl_performance <- tibble(my_estimate = c(glm_accuracy, glm_precision, 
                                          glm_recall, glm_f1score), 
                          type = c("Accuracy", "Precision", "Recall", "F1 Score"))

# load background image
img <-  "https://cdn.pixabay.com/photo/2019/11/01/05/57/ramen-4593402_1280.jpg"

ramen_plot <- ggplot(mdl_performance) +
  geom_col(aes(type, my_estimate), width = 0.1, 
           fill = "#FFC87E", colour = "black") +
  scale_y_continuous(NULL, expand = c(0, 0), limits = c(0,1)) +
  annotate(geom = "text", x = 2.5, y = 0.25, label = "25%", 
           colour = "#FFC87E", size = 3.5, fontface =2) +
  annotate(geom = "text", x = 2.5, y = 0.5, label = "50%", 
           colour = "#FFC87E", size = 3.5, fontface =2) +
  annotate(geom = "text", x = 2.5, y = 0.75, label = "75%", 
           colour = "#FFC87E", size = 3.5, fontface =2) +
  annotate(geom = "text", x = 2.5, y = 1, label = "100%", 
           colour = "#FFC87E", size = 3.5, fontface =2) +
  coord_polar() +
  theme_void() +
  theme(panel.grid.major.y = element_line(colour = "#FCFAF1", 
                                          linetype = "dotted", size = 1.2), 
        axis.text.x = element_text(colour = "#E34F33", face = "bold"), 
        plot.title = element_text(colour = "#FCFAF1")) +
  labs(title = "Ramen rating prediction performance")  

ramen_plot <- ggbackground(ramen_plot, img)

ggsave(plot = ramen_plot, filename = "ramen_plot.png")
