## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----setup---------------------------------------------------------------
library(nhhr)
library(rsample)
library(yardstick)
library(tidyverse)

## ------------------------------------------------------------------------
credit_df <- nhhr::credit_df

## ------------------------------------------------------------------------
split <- rsample::initial_split(credit_df, prop = 0.66)
train <- rsample::training(split)
test <- rsample::testing(split)

## ------------------------------------------------------------------------
linear_model <- lm(Duration_months ~ Age, data = train)
summary(linear_model)

## ------------------------------------------------------------------------
test$Predicted_duration <- predict(linear_model, newdata = test)

ggplot(test, aes(y = Predicted_duration, x = Age)) +
  geom_line(color = "blue") +
  theme_minimal()


## ------------------------------------------------------------------------

ggplot(test, aes(y = Predicted_duration, x = Duration_months)) +
  geom_point() +
  geom_abline(color = "red", linetype = "dashed") +
  expand_limits(x = c(0, 60), y = c(0, 60)) +
  theme_minimal()

## ------------------------------------------------------------------------
rmse <- test %>%
  yardstick::rmse(truth = Duration_months, estimate = Predicted_duration)

rsq <- test %>%
  yardstick::rsq(truth = Duration_months, estimate = Predicted_duration)

list(rmse = rmse$.estimate, rsquared = rsq$.estimate)

## ------------------------------------------------------------------------
linear_model <- lm(Duration_months ~ 
                     Gender_marital
                   + Age
                   + Occupation
                   + Foreign_worker, 
                   data = train)

# summary(linear_model)

## ------------------------------------------------------------------------
linear_model <- lm(Duration_months ~ 
                     Gender_marital * Occupation
                   + poly(Age, 2)
                   + Foreign_worker, 
                   data = train)

# broom::glance is an alternative to summary(model) if you just want the overall results formatted in a nice table
broom::glance(linear_model)

## ------------------------------------------------------------------------
ggplot(credit_df, aes(x = Age, y = Credit_amount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(formula = y ~ x, method = "lm") +
  scale_y_log10() +
  theme_minimal()

ggplot(credit_df, aes(x = Age, y = Credit_amount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(formula = y ~ poly(x, 2), method = "lm") +
  scale_y_log10() +
  theme_minimal()

