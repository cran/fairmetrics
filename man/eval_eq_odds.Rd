% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fairness.R
\name{eval_eq_odds}
\alias{eval_eq_odds}
\title{Examine Equalized Odds of a Predictive Model}
\usage{
eval_eq_odds(
  data,
  outcome,
  group,
  probs,
  cutoff = 0.5,
  confint = TRUE,
  bootstraps = 2500,
  alpha = 0.05,
  digits = 2,
  message = TRUE
)
}
\arguments{
\item{data}{A data frame containing the true binary outcomes, predicted
probabilities, and sensitive group membership.}

\item{outcome}{A string specifying the name of the binary outcome variable in
\code{data}.}

\item{group}{A string specifying the name of the binary sensitive attribute
variable (e.g., race, gender) used to define the comparison groups.}

\item{probs}{A string specifying the name of the variable containing
predicted probabilities or risk scores.}

\item{cutoff}{A numeric value used to threshold predicted probabilities into
binary predictions; defaults to 0.5.}

\item{confint}{Whether to compute 95\% confidence interval, default is TRUE.}

\item{bootstraps}{An integer specifying the number of bootstrap resamples for
constructing confidence intervals; vdefaults to 2500.}

\item{alpha}{Significance level for the (1 - \code{alpha}) confidence
interval; defaults to 0.05.}

\item{digits}{Number of decimal places to round numeric results; defaults to
2.}

\item{message}{Logical; if TRUE (default), prints a textual summary of the
fairness evaluation. Only works if \code{confint} is TRUE.}
}
\value{
A data frame summarizing group disparities in both FNR and FPR with
the following columns:
\itemize{
\item \code{Metric}: The reported metrics ("FNR; FPR").
\item \code{Group1}: Estimated FNR and FPR for the first group.
\item \code{Group2}: Estimated FNR and FPR for the second group.
\item \code{Difference}: Differences in FNR and FPR, computed as Group1 -
Group2.
\item \code{95\% CR}: Bonferroni-adjusted confidence regions for the
differences.
\item \code{Ratio}: Ratios in FNR and FPR, computed as Group1 / Group2.
\item \code{95\% CR}: Bonferroni-adjusted confidence regions for the ratios.
}
}
\description{
This function evaluates whether a predictive model satisfies the Equalized
Odds criterion by comparing both False Negative Rates (FNR) and False
Positive Rates (FPR) across two groups defined by a binary sensitive
attribute. It reports the rate for each group, their differences, ratios, and
bootstrap-based confidence regions. A Bonferroni-corrected union test is used
to test whether the model violates the Equalized Odds criterion.
}
\examples{
\donttest{
library(fairmetrics)
library(dplyr)
library(magrittr)
library(randomForest)
data("mimic_preprocessed")
set.seed(123)
train_data <- mimic_preprocessed \%>\%
  dplyr::filter(dplyr::row_number() <= 700)
# Fit a random forest model
rf_model <- randomForest::randomForest(factor(day_28_flg) ~ .,
  data = train_data, ntree = 1000
)
# Test the model on the remaining data
test_data <- mimic_preprocessed \%>\%
  dplyr::mutate(gender = ifelse(gender_num == 1, "Male", "Female")) \%>\%
  dplyr::filter(dplyr::row_number() > 700)

test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

# Fairness evaluation
# We will use sex as the sensitive attribute and day_28_flg as the outcome.
# We choose threshold = 0.41 so that the overall FPR is around 5\%.

# Evaluate Equalized Odds
eval_eq_odds(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
)
}
}
