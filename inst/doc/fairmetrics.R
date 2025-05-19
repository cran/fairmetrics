## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
# Packages we are using for the analysis
library(dplyr)
library(corrplot)
library(randomForest)
library(pROC)
library(SpecsVerification)
library(kableExtra)
library(naniar)
# Our package
library(fairmetrics)

## -----------------------------------------------------------------------------
# Loading mimic dataset 
# (available in fairmetrics)
data("mimic") 

missing_data_summary<- naniar::miss_var_summary(mimic, digits= 3)

kableExtra::kable(missing_data_summary, booktabs = TRUE, escape = FALSE) %>%
  kableExtra::kable_styling(
    latex_options = "hold_position"
  )


## -----------------------------------------------------------------------------
# Remove columns with more than 10% missing values
columns_to_remove <- missing_data_summary %>%
  dplyr::filter(pct_miss > 10) %>%
  dplyr::pull(variable)
  
mimic <- dplyr::select(mimic, 
                       -dplyr::one_of(columns_to_remove)
                       )

# Impute remaining missing values with median
mimic <- mimic %>% 
  dplyr::mutate(
    dplyr::across(
      dplyr::where(~any(is.na(.))), 
                  ~ifelse(is.na(.), median(., na.rm = TRUE), .)
                  )
    )

## -----------------------------------------------------------------------------
# Identify columns that have only one unique value
cols_with_one_value <- sapply(mimic, function(x) length(unique(x)) == 1)
# Subset the dataframe to remove these columns
mimic <- mimic[, !cols_with_one_value]

## -----------------------------------------------------------------------------
# Remove columns that are highly correlated with the outcome variable
corrplot::corrplot(cor(select_if(mimic, is.numeric)), method = "color", tl.cex = 0.5)

mimic <- mimic %>% 
  dplyr::select(-c("hosp_exp_flg", "icu_exp_flg", "mort_day_censored", "censor_flg"))

## -----------------------------------------------------------------------------
# Use 700 labels to train the mimic
train_data <- mimic %>% 
  dplyr::filter(
    dplyr::row_number() <= 700
    )

# Fit a random forest model
set.seed(123)
rf_model <- randomForest::randomForest(factor(day_28_flg) ~ ., data = train_data, ntree = 1000)

# Test the model on the remaining data
test_data <- mimic %>% 
  dplyr::filter(
    dplyr::row_number() > 700
    )

test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[,2]

# Check the AUC
roc_obj <- pROC::roc(test_data$day_28_flg, test_data$pred)
roc_auc <- pROC::auc(roc_obj)
roc_auc

## -----------------------------------------------------------------------------
# Recode gender variable explicitly for readability: 

test_data <- test_data %>%
  dplyr::mutate(gender = ifelse(gender_num == 1, "Male", "Female"))

## -----------------------------------------------------------------------------
# Control the overall false positive rate (FPR) at 5% by setting a threshold.

cut_off <- 0.41

test_data %>%
  dplyr::mutate(pred = ifelse(pred > cut_off, 1, 0)) %>%
  dplyr::filter(day_28_flg == 0) %>%
  dplyr::summarise(fpr = mean(pred))

## -----------------------------------------------------------------------------
fairness_result <- fairmetrics::get_fairness_metrics(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  group2 = "age",
  condition = ">=60",
  probs = "pred",
  cutoff = cut_off
 )

kableExtra::kable(fairness_result, booktabs = TRUE, escape = FALSE) %>%
  kableExtra::kable_styling(full_width = FALSE) %>%
  kableExtra::pack_rows("Independence-based criteria", 1, 2) %>%
  kableExtra::pack_rows("Separation-based criteria", 3, 6) %>%
  kableExtra::pack_rows("Sufficiency-based criteria", 7, 7) %>%
  kableExtra::pack_rows("Other criteria", 8, 10) %>%
  kableExtra::kable_styling(
    full_width = FALSE,
    font_size = 10,         # Controls font size manually
    latex_options = "hold_position"
  )

