library(tidyverse)
iris <- iris %>%
  as_tibble() %>%
  # Add ID column:
  mutate(ID = 1:n()) %>% 
  select(ID, Species, Sepal.Length, Sepal.Width)

library(rpart)
model_formula <- as.formula(Species ~ Sepal.Length + Sepal.Width)
tree_parameters <- rpart.control(maxdepth = 3)
model_CART <- rpart(model_formula, data = iris, control=tree_parameters)

# Plot
plot(model_CART, margin=0.25)
text(model_CART, use.n = TRUE)
title("Predicting iris species using sepal length & width")
box()

p_hat_matrix <- model_CART %>% 
  predict(type = "prob", newdata = iris)

# Look at a random sample of 5 of them
p_hat_matrix %>% 
  as_tibble() %>% 
  sample_n(5)

MLmetrics::MultiLogLoss(y_true = iris$Species, y_pred = p_hat_matrix)

