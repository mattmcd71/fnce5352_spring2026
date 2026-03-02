# ============================================================
# FNCE 5352 — Lecture 6 Live Demo (SIMPLIFIED)
# Resampling (Default) + KNN tuning (Caravan)
#
# Philosophy:
# - Keep the modeling in base R where possible (glm)
# - Use rsample only to *make splits/folds*
# - Use yardstick only to *compute AUC*
# - Keep code readable for folks new-ish to R
# ============================================================

library(dplyr)
library(rsample)
library(yardstick)
library(class)   # knn()
library(tidyr)

# If you use ISLR (not ISLR2), just change this line to: library(ISLR)
library(ISLR2)

set.seed(1)

# ============================================================
# PART A — Default: one split is noisy + 10-fold CV (logistic)
# ============================================================


df <- Default  # `default` is already a factor in ISLR/ISLR2

# --- A1) Five different random splits: watch AUC move around ---
auc_vec <- numeric(5)

for (s in 1:5) {
  set.seed(s)

  sp <- initial_split(df, prop = 0.7, strata = default)
  train <- training(sp)
  test  <- testing(sp)

  fit <- glm(default ~ balance, data = train, family = binomial())

  scored <- test %>%
    mutate(p_hat = predict(fit, newdata = test, type = "response"))

  # Compute AUC and extract the number
  auc_vec[s] <- roc_auc(scored, truth = default, p_hat, event_level = "second") %>%
    pull(.estimate)
}

tibble(split_seed = 1:5, auc = auc_vec)

# Takeaway: one split is noisy. Now do 10-fold CV on the training set

# --- A2) Train/Test once; CV only inside training ---
set.seed(123)

sp0 <- initial_split(df, prop = 0.7, strata = default)
train0 <- training(sp0)
test0  <- testing(sp0)

folds0 <- vfold_cv(train0, v = 10, strata = default)

fold_auc <- numeric(nrow(folds0))

for (i in 1:nrow(folds0)) {
  split_i <- folds0$splits[[i]]
  tr <- analysis(split_i)
  va <- assessment(split_i)

  fit <- glm(default ~ balance, data = tr, family = binomial())

  scored <- va %>%
    mutate(p_hat = predict(fit, newdata = va, type = "response"))

  fold_auc[i] <- roc_auc(scored, truth = default, p_hat, event_level = "second") %>%
    pull(.estimate)
}

cv_summary <- tibble(fold = 1:length(fold_auc), auc = fold_auc)
cv_summary

# CV mean / SD:
cv_summary %>% summarise(mean_auc = mean(auc), sd_auc = sd(auc))

# --- Optional: final one-time test AUC (after we've chosen the model) ---
fit_final <- glm(default ~ balance, data = train0, family = binomial())
scored_test <- test0 %>% mutate(p_hat = predict(fit_final, newdata = test0, type = "response"))

test_auc <- roc_auc(scored_test, truth = default, p_hat, event_level = "second") %>%
  pull(.estimate)

# Final one-time TEST AUC
test_auc

# ============================================================
# PART B — Caravan: KNN needs scaling + tune k with CV
# ============================================================

car <- Caravan 

# Use all predictors (everything except the outcome)
x_cols <- setdiff(names(car), "Purchase")

# --- B1) Single split: show training vs test AUC for different k ---
set.seed(456)

sp1 <- initial_split(car, prop = 0.7, strata = Purchase)
train1 <- training(sp1)
test1  <- testing(sp1)

# Build raw X matrices
X_train_raw <- as.matrix(train1[, x_cols])
X_test_raw  <- as.matrix(test1[, x_cols])

y_train <- train1$Purchase
y_test  <- test1$Purchase

# Scale using TRAIN stats only (avoid leakage)
mu  <- colMeans(X_train_raw)
sdv <- apply(X_train_raw, 2, sd)
sdv[sdv == 0] <- 1

X_train <- scale(X_train_raw, center = mu, scale = sdv)
X_test  <- scale(X_test_raw, center = mu, scale = sdv)

k_show <- c(1, 3, 11, 51, 101)

train_auc <- numeric(length(k_show))
test_auc  <- numeric(length(k_show))

for (j in seq_along(k_show)) {
  k <- k_show[j]

  # --- Training AUC (optimistic on purpose) ---
  pred_tr <- knn(train = X_train, test = X_train, cl = y_train, k = k, prob = TRUE)
  p_win_tr <- attr(pred_tr, "prob")
  p_hat_tr <- ifelse(pred_tr == "Yes", p_win_tr, 1 - p_win_tr)

  scored_tr <- tibble(Purchase = y_train, p_hat = p_hat_tr)
  train_auc[j] <- roc_auc(scored_tr, truth = Purchase, p_hat, event_level = "second") %>%
    pull(.estimate)

  # --- Test AUC (what we care about) ---
  pred_te <- knn(train = X_train, test = X_test, cl = y_train, k = k, prob = TRUE)
  p_win_te <- attr(pred_te, "prob")
  p_hat_te <- ifelse(pred_te == "Yes", p_win_te, 1 - p_win_te)

  scored_te <- tibble(Purchase = y_test, p_hat = p_hat_te)
  test_auc[j] <- roc_auc(scored_te, truth = Purchase, p_hat, event_level = "second") %>%
    pull(.estimate)
}

tibble(k = k_show, auc_train = train_auc, auc_test = test_auc)
# 
Takeaway: small k can look great in-sample. We pick k using CV

# --- B2) 10-fold CV (inside training) to tune k ---
set.seed(789)

folds1 <- vfold_cv(train1, v = 10, strata = Purchase)
k_grid <- c(1, 3, 5, 11, 25, 51, 101, 201, 401, 3600)

# We'll store fold AUCs in a matrix: rows = folds, cols = k values
auc_mat <- matrix(NA_real_, nrow = nrow(folds1), ncol = length(k_grid))
colnames(auc_mat) <- paste0("k=", k_grid)

for (i in 1:nrow(folds1)) {
  split_i <- folds1$splits[[i]]
  tr <- analysis(split_i)
  va <- assessment(split_i)

  # Make matrices for this fold
  X_tr_raw <- as.matrix(tr[, x_cols])
  X_va_raw <- as.matrix(va[, x_cols])
  y_tr <- tr$Purchase
  y_va <- va$Purchase

  # Scale using THIS fold's training stats
  mu  <- colMeans(X_tr_raw)
  sdv <- apply(X_tr_raw, 2, sd)
  sdv[sdv == 0] <- 1

  X_tr <- scale(X_tr_raw, center = mu, scale = sdv)
  X_va  <- scale(X_va_raw, center = mu, scale = sdv)

  # Try each k
  for (j in seq_along(k_grid)) {
    k <- k_grid[j]

    pred <- knn(train = X_tr, test = X_va, cl = y_tr, k = k, prob = TRUE)
    p_win <- attr(pred, "prob")
    p_hat <- ifelse(pred == "Yes", p_win, 1 - p_win)

    scored <- tibble(Purchase = y_va, p_hat = p_hat)

    auc_mat[i, j] <- roc_auc(scored, truth = Purchase, p_hat, event_level = "second") %>%
      pull(.estimate)
  }
}

# Summarize mean + SD across folds for each k
mean_auc <- colMeans(auc_mat, na.rm = TRUE)
sd_auc   <- apply(auc_mat, 2, sd, na.rm = TRUE)

tune_tbl <- tibble(
  k = k_grid,
  mean_auc = as.numeric(mean_auc),
  sd_auc = as.numeric(sd_auc)
) %>% arrange(desc(mean_auc))

print(tune_tbl)

best_k <- tune_tbl$k[1]
cat("\nBest k by mean CV AUC:", best_k, "\n")

# --- B3) Final one-time test AUC using best_k ---
pred_best <- knn(train = X_train, test = X_test, cl = y_train, k = best_k, prob = TRUE)
p_win_best <- attr(pred_best, "prob")
p_hat_best <- ifelse(pred_best == "Yes", p_win_best, 1 - p_win_best)

scored_best <- tibble(Purchase = y_test, p_hat = p_hat_best)
test_auc_best <- roc_auc(scored_best, truth = Purchase, estimate = p_hat, event_level = "second") %>%
  pull(.estimate)

cat("\nFinal one-time TEST AUC for tuned KNN:\n")
print(test_auc_best)

cat("\nDone.\n")
