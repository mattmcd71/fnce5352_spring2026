# ============================================================
# FNCE 5352 — Lecture 5 Live Demo (R)
# Classification + Logistic Regression (Credit Risk Framing)
#
# Dataset: ISLR2::Default (classic "default risk" example)
# Model: glm(..., family = binomial)  [base R]
# Metrics: yardstick (calculator only)
#
# Teaching goals:
#  1) Probability output: P(default = "Yes" | X)
#  2) Threshold -> confusion matrix -> tradeoffs
#  3) ROC/AUC as threshold-free summary
# ============================================================

# ---- Setup ----
library(tidyverse)
library(ISLR2)
library(yardstick)

set.seed(5352)

# ---- Load + quick look ----
df <- Default %>%
  mutate(
    default = factor(default, levels = c("No", "Yes")),
    student = factor(student),
    # a simple derived feature: utilization-ish proxy (demo only)
    bal_k = balance / 1000
  )

glimpse(df)
df %>% count(default) %>% mutate(p = n / sum(n))

# ---- Train/test split ----
# (Not time series here; just a simple random split for demonstration.)
n <- nrow(df)
train_idx <- sample.int(n, size = floor(0.8 * n))

train <- df[train_idx, ]
test  <- df[-train_idx, ]

train %>% count(default) %>% mutate(p = n / sum(n))
test  %>% count(default) %>% mutate(p = n / sum(n))

train %>% count(default) %>% mutate(p = n / sum(n))
test %>% count(default) %>% mutate(p = n / sum(n))

# ---- Baseline model: always predict "No" ----
baseline <- test %>%
  mutate(
    p_hat = 0,
    pred  = factor("No", levels = levels(default))
  )

baseline %>% accuracy(truth = default, estimate = pred)

# ---- Fit logistic regression (base R) ----
# Start small: balance only
fit1 <- glm(default ~ balance, data = train, family = binomial)
summary(fit1)

# Add a few predictors (classic version)
fit2 <- glm(default ~ balance + income + student, data = train, family = binomial)
summary(fit2)

# ---- Predict probabilities on test set ----
test_scored <- test %>%
  mutate(
    p_hat = predict(fit2, newdata = test, type = "response")
  )

test_scored %>%
  summarise(min_p = min(p_hat), mean_p = mean(p_hat), max_p = max(p_hat))

# ---- Helper: metrics at a threshold ----
metrics_at_threshold <- function(scored_df, threshold = 0.5) {
  scored_df %>%
    mutate(
      pred = if_else(p_hat > threshold, "Yes", "No"),
      pred = factor(pred, levels = levels(default))
    ) %>%
    summarise(
      accuracy  = accuracy_vec(truth = default, estimate = pred),
      sens      = sens_vec(truth = default, estimate = pred),
      spec      = spec_vec(truth = default, estimate = pred)
    )
}

# Evaluate at 0.5
metrics_at_threshold(test_scored, 0.5)

# Confusion matrix at 0.5
test_scored %>%
  mutate(
    pred = if_else(p_hat > 0.5, "Yes", "No"),
    pred = factor(pred, levels = levels(default))
  ) %>%
  conf_mat(truth = default, estimate = pred)

# ---- Vary the threshold ----
threshold_grid <- tibble(threshold = seq(0.05, 0.95, by = 0.05))

threshold_results <- threshold_grid %>%
  mutate(out = map(threshold, ~ metrics_at_threshold(test_scored, .x))) %>%
  unnest(out)

threshold_results

# Quick plot to visualize tradeoff (no custom styling)
threshold_results %>%
  pivot_longer(cols = c(accuracy, sens, spec), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = threshold, y = value)) +
  geom_line() +
  facet_wrap(~ metric) +
  labs(title = "Threshold tradeoffs (Default dataset)", y = "metric value")

# ---- ROC + AUC (yardstick) ----
# yardstick expects the probability column to correspond to the "event" level.
# default has levels c("No","Yes"), so event = "Yes".
roc_obj <- test_scored %>%
  roc_curve(truth = default, p_hat, event_level = "second")

roc_auc_val <- test_scored %>%
  roc_auc(truth = default, p_hat, event_level = "second")

autoplot(roc_obj) + labs(title = "ROC curve — logistic regression on Default")

test_scored <- test_scored %>%
  mutate(
    p_hat_simple = predict(fit1, newdata = test, type = "response")
  )

roc_obj2 <- test_scored %>%
  roc_curve(truth = default, p_hat_simple, event_level = "second")

roc_auc_val2 <- test_scored %>%
  roc_auc(truth = default, p_hat_simple, event_level = "second")

autoplot(roc_obj2) + labs(title = "ROC curve — logistic regression on Default")
