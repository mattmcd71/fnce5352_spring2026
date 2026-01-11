# ============================================================
# FNCE 5352 — Lecture 1 Live Demo (R)
# Intro to R, RStudio, and the Data Science Workflow (Finance-first)
#
# This script is designed to be run line-by-line in class.
# It starts with a gentle intro to R programming, then moves into
# a real finance-flavored workflow using AAPL + SPY data.
#
# Expected file:
#   data/aapl_spy.csv
# Columns (from tidyquant is perfect):
#   date, ticker, adj_close
# ============================================================


# -----------------------------
# 0) Packages (install once, load each session)
# -----------------------------

# If needed (run once per computer):
# install.packages(c("tidyverse", "here", "lubridate", "scales"))

library(tidyverse)
library(here)
library(lubridate)
library(scales)


# -----------------------------
# 1) R basics: calculator + functions
# -----------------------------

3 + 3
sqrt(2)

# Functions can be nested
sqrt(exp(sqrt(2)))

# Get help on a function
?sqrt


# -----------------------------
# 2) Objects: assign values
# -----------------------------

x <- 3 + 5
x

# A vector (a collection of values)
y <- c(1, 2, 3, 4, 5)
y

# Create a sequence
seq(from = 0, to = 10, by = 2)

# Inspect the structure of an object
str(y)


# -----------------------------
# 3) Write a simple function
# -----------------------------

add_pi <- function(x) {
  x + 3.14
}

add_pi(3)
add_pi(y)


# -----------------------------
# 4) Pipes: readable, step-by-step workflows
# -----------------------------

# Nested version:
sqrt(exp(sqrt(2)))

# Pipe version (read left-to-right):
2 |>
  sqrt() |>
  exp() |>
  sqrt()

# Note: We'll use the base pipe |> in this course.
# You may also see %>% in older tidyverse materials.


# ============================================================
# 5) Finance workflow demo: AAPL + SPY
# Import → Tidy → Explore → (Teaser) Model → Communicate
# ============================================================

# -----------------------------
# 5.1) Import: load the data
# -----------------------------

data_path <- here("Lecture1", "data", "aapl_spy.csv")
data_path  # shows the resolved path (useful for debugging)

px_raw <- read_csv(data_path, show_col_types = FALSE)
glimpse(px_raw)

# If your file is somewhere else, adjust data_path, e.g.:
# data_path <- here("Lecture1", "aapl_spy.csv")


# -----------------------------
# 5.2) Tidy: clean columns and build a "working" table
# -----------------------------

# We standardize to: date, ticker, price
px <- px_raw |>
  mutate(
    date   = as.Date(date),
    ticker = as.character(ticker),
    price  = adj_close
  ) |>
  select(date, ticker, price) |>
  arrange(ticker, date)

stopifnot(all(c("date", "ticker", "price") %in% names(px)))

px |>
  slice_head(n = 5)

# A tibble is a modern data frame
class(px)


# -----------------------------
# 5.3) Explore: prices over time
# -----------------------------

px |>
  ggplot(aes(x = date, y = price, color = ticker)) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "AAPL and SPY - Prices Over Time",
    x = NULL, y = "Price"
  )


# -----------------------------
# 5.4) Transform: compute simple daily returns
# -----------------------------

ret <- px |>
  group_by(ticker) |>
  mutate(ret = price / lag(price) - 1) |>
  ungroup()

ret |>
  filter(!is.na(ret)) |>
  slice_head(n = 5)


# -----------------------------
# 5.5) Explore: return distributions
# -----------------------------

ret |>
  filter(!is.na(ret)) |>
  ggplot(aes(x = ret, fill = ticker)) +
  geom_histogram(bins = 60, alpha = 0.6, position = "identity") +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Daily Return Distributions (Simple Returns)",
    x = "Daily return", y = "Count"
  )


# -----------------------------
# 5.6) Communicate: a quick summary table
# -----------------------------

ret |>
  group_by(ticker) |>
  summarize(
    n_days   = sum(!is.na(ret)),
    mean_ret = mean(ret, na.rm = TRUE),
    sd_ret   = sd(ret, na.rm = TRUE),
    .groups  = "drop"
  )


# -----------------------------
# 5.7) Teaser model (optional): AAPL ~ SPY
# -----------------------------
# This is a preview of what "modeling" looks like in the workflow.
# We are not doing regression theory today.

wide <- ret |>
  select(date, ticker, ret) |>
  filter(!is.na(ret)) |>
  pivot_wider(names_from = ticker, values_from = ret)

# Simple market model / CAPM-style regression
fit <- lm(AAPL ~ SPY, data = wide)
summary(fit)


# -----------------------------
# 6) Mini exercise ideas (optional)
# -----------------------------
# These are meant to be simple modifications students can try.

# Exercise A:
# Filter to a single year and remake the histogram.
# ret_2024 <- ret |>
#   filter(date >= as.Date("2024-01-01"), date <= as.Date("2024-12-31"))
#
# ret_2024 |>
#   filter(!is.na(ret)) |>
#   ggplot(aes(x = ret, fill = ticker)) +
#   geom_histogram(bins = 60, alpha = 0.6, position = "identity") +
#   scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
#   labs(title = "Daily Returns in 2024", x = "Daily return", y = "Count")

# Exercise B:
# Find the worst 5 AAPL daily returns.
# ret |>
#   filter(ticker == "AAPL", !is.na(ret)) |>
#   arrange(ret) |>
#   slice_head(n = 5)


# -----------------------------
# 7) Cleanup (optional)
# -----------------------------
# rm(list = ls())
