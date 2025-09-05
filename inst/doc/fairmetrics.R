## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(123)

## ----message=FALSE, warning=FALSE---------------------------------------------
# Load required libraries
library(dplyr)
library(fairmetrics)
library(randomForest)
# Set seed for reproducibility
set.seed(1)
# Use 700 labels to train on the mimic_preprocessed dataset
train_data <- mimic_preprocessed %>%
  filter(row_number() <= 700)

# Test the model on the remaining data
test_data <- mimic_preprocessed %>%
  filter(row_number() > 700)

# Fit a random forest model
rf_model <- randomForest(factor(day_28_flg) ~ ., data = train_data, ntree = 1000)
# Save model prediction
test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

## -----------------------------------------------------------------------------
# Recode gender variable explicitly for readability: 
test_data <- test_data %>%
  mutate(gender = ifelse(gender_num == 1, "Male", "Female"))

## -----------------------------------------------------------------------------
eval_stats_parity(
  data = test_data, 
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41,
  message = TRUE
)

## -----------------------------------------------------------------------------
get_fairness_metrics(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
 )

## -----------------------------------------------------------------------------
eval_stats_parity(
  data = subset(test_data, age >= 60), 
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41,
  message = TRUE
)

## -----------------------------------------------------------------------------
get_fairness_metrics(
  data = subset(test_data, age >= 60), 
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
)

