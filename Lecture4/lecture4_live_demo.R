# ============================================================
# FNCE 5352 — Lecture 4 Live Demo (R)
# Linear Regression: Introduction & Interpretation
#
# Style:
# - Base R modeling: lm()
# - Interpretation + model assessment habits
# - Yardstick used as a *metrics calculator* (not a full tidymodels workflow)
#
# Expected data options (use whatever exists in your repo):
# - data/aapl_spy.csv (Date, AAPL, SPY) OR (date, aapl, spy)
# - Optional: data/vix.csv (Date, VIX) OR include VIX in main file
#
# ============================================================

# ---- setup ----
library(tidyverse)
library(lubridate)
library(yardstick)
library(ISLR2)

# ---- helpers ----
rmse_base <- function(truth, estimate) {
  sqrt(mean((truth - estimate)^2, na.rm = TRUE))
}

mse_base <- function(truth, estimate) {
  mean((truth - estimate)^2, na.rm = TRUE)
}

rsq_base <- function(truth, estimate) {
  ss_res <- sum((truth - estimate)^2, na.rm = TRUE)
  ss_tot <- sum((truth - mean(truth, na.rm = TRUE))^2, na.rm = TRUE)
  1 - ss_res/ss_tot
}

metric_report <- function(truth, estimate) {

  tibble(
    rmse = rmse_base(truth, estimate),
    mse  = mse_base(truth, estimate),
    rsq  = rsq_base(truth, estimate)
  )
}

# Simple return function (choose one; keep it consistent)
ret_simple <- function(x) (x / dplyr::lag(x) - 1)

# ---- PART A: Advertising dataset (ISLR2) ----

Advertising <- readr::read_csv(here::here("Lecture4", "data", "Advertising.csv"))[,2:5]

# Quick look
Advertising |> glimpse()

# Simple regression: Sales ~ TV
# TYPE LIVE
m_adv1 <- lm(sales ~ TV, data = Advertising)
summary(m_adv1)
confint(m_adv1)

  # Minimal plot: scatter + fitted line
  # TYPE LIVE (keep visuals quick)
  plot(Advertising$TV, Advertising$sales,
       xlab = "TV ad spend", ylab = "Sales")
  abline(m_adv1, lwd = 2)

  # Residual pattern check
  # TYPE LIVE
  plot(fitted(m_adv1), resid(m_adv1),
       xlab = "Fitted", ylab = "Residuals")
  abline(h = 0, lty = 2)


library(ggplot2)

ggplot(Advertising, aes(x = TV, y = sales)) +
  geom_point() +
  geom_abline(
    intercept = coef(m_adv1)[1],
    slope     = coef(m_adv1)[2]
  ) +
  labs(x = "TV ad spend", y = "Sales")

library(broom)

augment(m_adv1) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Fitted", y = "Residuals")


  # Multiple regression: Sales ~ TV + Radio + Newspaper
  # TYPE LIVE
  m_adv2 <- lm(sales ~ TV + radio + newspaper, data = Advertising)
  summary(m_adv2)

  # Interpret partial slopes: "holding other channels constant"
  # (talk track; no extra code needed)

  # Quick performance (random split is OK here; not a time series)
  set.seed(5352)
  n <- nrow(Advertising)
  idx_train <- sample.int(n, size = floor(0.7*n))
  adv_train <- Advertising[idx_train, ]
  adv_test  <- Advertising[-idx_train, ]

  m_adv2_tr <- lm(sales ~ TV + radio + newspaper, data = adv_train)

  summary(m_adv2_tr)
  pred_adv2 <- predict(m_adv2_tr, newdata = adv_test)

  metric_report(truth = adv_test$sales, estimate = pred_adv2)


# ---- PART B: Finance data (AAPL/SPY, optional VIX) ----
# TYPE LIVE: "Now we do the same thing with finance meaning"

# 1) Load price data
# Expect a file like data/aapl_spy.csv
# Columns: date (or Date), AAPL (or aapl), SPY (or spy)
price_path <- here::here('Lecture4', 'data', 'aapl_spy_vix.csv')

prices <- readr::read_csv(price_path, show_col_types = FALSE)

prices <- prices |>
  mutate(date = as.Date(date)) |>
  pivot_wider(names_from = ticker, 
              values_from = adj_close) |>
  rename(VIX = `^VIX`)

#colnames(prices) <- c('date', 'AAPL', 'SPY', 'VIX')

# 3) Compute returns (daily)
rets <- prices |>
  mutate(
    aapl_ret = ret_simple(AAPL),
    spy_ret  = ret_simple(SPY),
    vix_change = VIX - lag(VIX))  |>
  filter(!is.na(aapl_ret), !is.na(spy_ret), !is.na(vix_change))

rets |> summarise(
  n = n(),
  start = min(date),
  end = max(date),
  have_vix = any(!is.na(vix_change))
)

# 4) Simple regression: market model
# TYPE LIVE
m1 <- lm(aapl_ret ~ spy_ret, data = rets)
summary(m1)
confint(m1)

# Visual: scatter + fitted line
# TYPE LIVE
plot(rets$spy_ret, rets$aapl_ret,
     xlab = "SPY return", ylab = "AAPL return")
abline(m1, lwd = 2)

# Residuals: pattern check
# TYPE LIVE
plot(fitted(m1), resid(m1),
     xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2)

#what if we remove the 568th point in the data set?
m1a <- lm(aapl_ret ~ spy_ret, data = rets[-568,])
summary(m1a)

plot(rets$spy_ret[-568], rets$aapl_ret[-568],
     xlab = "SPY return", ylab = "AAPL return")
abline(m1a, lwd = 2)

# 5) Multiple regression: add VIX (if available)
  # TYPE LIVE
  m2 <- lm(aapl_ret ~ spy_ret + vix_change, data = rets)
  summary(m2)

  # Interpret partial slopes (talk track):
  # - beta_m: market sensitivity controlling for VIX changes
  # - beta_v: response to volatility changes holding market fixed

  # Quick check: correlation between predictors
  cor(rets$spy_ret, rets$vix_change, use = "complete.obs")


# TYPE LIVE: "train on past, test on future"
N <- nrow(rets)
cut <- floor(0.7 * N)
train <- rets[1:cut, ]
test  <- rets[(cut+1):N, ]

m1_tr <- lm(aapl_ret ~ spy_ret, data = train)
pred_test <- predict(m1_tr, newdata = test)

metric_report(truth = test$aapl_ret, estimate = pred_test)

# Optional: out-of-sample R^2 as "variance explained on test"
# (warn: can be negative)

# 7) If VIX exists, do the same for multiple regression
  train2 <- train |> filter(!is.na(vix_change))
  test2  <- test  |> filter(!is.na(vix_change))

  m2_tr <- lm(aapl_ret ~ spy_ret + vix_change, data = train2)
  pred2 <- predict(m2_tr, newdata = test2)

  metric_report(truth = test2$aapl_ret, estimate = pred2)


# ---- Wrap prompts (for class discussion) ----
# 1) What does beta mean in units?
# 2) What does alpha mean (and when is it not meaningful)?
# 3) What do residual patterns suggest in finance?
# 4) Which metric do you trust most, and why?
