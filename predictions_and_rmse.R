#######################################
# generate predictions and compute RMSE
#######################################

library(tidyverse)

# load previously wrangled training set and validation set

load("rdas/edx.rda")
load("rdas/validation.rda")

# compute mean rating for training set

mu <- mean(edx$rating)

# use training set to create tibble with estimates of movie effects

m_effect <- edx %>% group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# use training set to create tibble with estimates of user effects

u_effect <- edx %>% left_join(m_effect, by = "movieId") %>%
  group_by(userId) %>% summarize(b_u = mean(rating - b_i - mu))

# use training set estimates for mean rating, user effects and movie effects
# to generate predictions for test set

predicted <- validation %>% 
  left_join(m_effect, by = "movieId") %>%
  left_join(u_effect, by =  "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# create function to compute RMSE

RMSE <- function(actual, predicted){sqrt(mean((actual-predicted)^2))}

# compute RMSE for predictions

RMSE(validation$rating, predicted)




