# ============================================================
# FNCE 5352 — Lecture 7 Live Demo (R)
# Model selection + regularization + nonlinearity
#
# Philosophy:
# - Mostly base R (lm/glm) for clarity
# - We will use:
#     * rsample  : resampling splits/folds (NO full tidymodels workflow)
#     * yardstick: metrics (AUC/RMSE)
#     * leaps    : stepwise subset selection helper
#     * glmnet   : ridge/lasso (the one "must-have" package)
#     * splines  : spline bases (comes with R)
#     * mgcv     : optional GAM (skip if you want)
# ============================================================

# --------------------------
# 0) Setup
# --------------------------

# TYPE LIVE: load packages (keep it light)
suppressPackageStartupMessages({
  library(rsample)
  library(yardstick)
  library(glmnet)
  library(leaps)
  library(splines)
  # library(mgcv) # optional
  library(ISLR2)  # for Hitters, Default
})

set.seed(5352)

# Helper: RMSE for regression
rmse_vec <- function(truth, estimate) {
  sqrt(mean((truth - estimate)^2, na.rm = TRUE))
}

# Helper: AUC for binary classification with probs for event="Yes"
auc_yes <- function(truth_factor, prob_yes) {
  # yardstick expects a factor with levels c("No","Yes") (event = "second")
  yardstick::roc_auc_vec(truth = truth_factor, estimate = prob_yes, event_level = "second")
}

# Helper: make fold IDs from rsample vfold object
fold_ids <- function(vfold_obj) seq_along(vfold_obj$splits)

# ============================================================
# 1) Subset selection (regression) — Hitters example
# ============================================================
# Why Hitters? It has a bunch of predictors so model selection is interesting.
# (Finance translation: think "lots of candidate predictors".)

hit <- na.omit(Hitters)

# Response we'll predict
# Salary is in thousands
# TYPE LIVE: quick glance
# str(hit); summary(hit$Salary)

# ---- 1A) Stepwise selection using regsubsets (in-sample criteria)
# (This is NOT CV yet — we are using in-sample metrics like BIC/adjR2.)
regfit_fwd <- regsubsets(Salary ~ ., data = hit, nvmax = 19, method = "forward")
regfit_bwd <- regsubsets(Salary ~ ., data = hit, nvmax = 19, method = "backward")

sum_fwd <- summary(regfit_fwd)
sum_bwd <- summary(regfit_bwd)

# PRE-WRITTEN (safe): plots to discuss
par(mfrow = c(1, 3))
plot(sum_fwd$adjr2, xlab = "Model size", ylab = "Adjusted R^2", type = "b")
plot(sum_fwd$bic,   xlab = "Model size", ylab = "BIC", type = "b")
plot(sum_fwd$cp,    xlab = "Model size", ylab = "C_p", type = "b")
par(mfrow = c(1, 1))

# TYPE LIVE: show which variables are chosen at a given model size
coef(regfit_fwd, id = which.min(sum_fwd$bic))

# ---- 1B) CV for model size (simple, readable loop)
# IMPORTANT: selection must happen INSIDE each fold to avoid leakage.
set.seed(5352)
folds_hit <- vfold_cv(hit, v = 10)

# We'll evaluate model sizes 1..19
Kmax <- 19
cv_rmse <- matrix(NA_real_, nrow = Kmax, ncol = length(folds_hit$splits))

for (fold in fold_ids(folds_hit)) {
  sp <- folds_hit$splits[[fold]]
  train_df <- analysis(sp)
  test_df  <- assessment(sp)

  # Fit forward stepwise inside the fold
  regfit_fold <- regsubsets(Salary ~ ., data = train_df, nvmax = Kmax, method = "forward")

  for (k in 1:Kmax) {
    # Predict with the k-variable model on the fold's assessment set
    x_test <- model.matrix(Salary ~ ., data = test_df)
    beta_k <- coef(regfit_fold, id = k)
    preds <- x_test[, names(beta_k), drop = FALSE] %*% beta_k
    cv_rmse[k, fold] <- rmse_vec(test_df$Salary, preds)
  }
}

mean_rmse_by_k <- rowMeans(cv_rmse, na.rm = TRUE)
best_k <- which.min(mean_rmse_by_k)
best_k

plot(1:Kmax, mean_rmse_by_k, type = "b",
     xlab = "Model size (k predictors)",
     ylab = "10-fold CV RMSE")

# Talking point: k is a hyperparameter chosen by CV, just like KNN's k or ridge's lambda.

# ============================================================
# 2) Regularization — ridge vs lasso via glmnet
# ============================================================
# We'll do ridge/lasso on the SAME Hitters regression problem first.

X_hit <- model.matrix(Salary ~ ., data = hit)[, -1]  # drop intercept
y_hit <- hit$Salary

# Ridge: alpha = 0
set.seed(5352)
cv_ridge <- cv.glmnet(X_hit, y_hit, alpha = 0)  # gaussian is default
plot(cv_ridge)
cv_ridge$lambda.min
cv_ridge$lambda.1se

# Lasso: alpha = 1
set.seed(5352)
cv_lasso <- cv.glmnet(X_hit, y_hit, alpha = 1)
plot(cv_lasso)

# Show coefficient sparsity for lasso at lambda.1se
coef(cv_lasso, s = "lambda.1se")

# Discussion prompts:
# - Ridge shrinks, keeps everything (usually)
# - Lasso can set some coefficients exactly to 0 (sparse)
# - Correlated predictors: ridge shares; lasso tends to pick one

# ============================================================
# 3) Regularized logistic regression — Default (credit framing)
# ============================================================
df <- ISLR2::Default

# We'll predict default ("Yes"/"No") using a richer feature set than the raw 3 vars.
# TYPE LIVE: create some simple engineered features (illustrative!)
df$balance2 <- df$balance^2
df$log_income <- log(df$income)

# Interactions (careful: keep it small)
df$bal_x_student <- df$balance * ifelse(df$student == "Yes", 1, 0)

# Train/test split (hold out test ONCE)
set.seed(5352)
split <- initial_split(df, prop = 0.75, strata = default)
train <- training(split)
test  <- testing(split)

# Model matrix for glmnet (handles factors)
X_train <- model.matrix(default ~ student + balance + balance2 + log_income + bal_x_student, data = train)[, -1]
y_train <- train$default  # factor with levels No/Yes

X_test  <- model.matrix(default ~ student + balance + balance2 + log_income + bal_x_student, data = test)[, -1]
y_test  <- test$default

# Ridge logistic
set.seed(5352)
cv_ridge_logit <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0)
plot(cv_ridge_logit)

p_test_ridge <- as.numeric(predict(cv_ridge_logit, newx = X_test, s = "lambda.1se", type = "response"))
auc_yes(y_test, p_test_ridge)

# Lasso logistic
set.seed(5352)
cv_lasso_logit <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1)
plot(cv_lasso_logit)

p_test_lasso <- as.numeric(predict(cv_lasso_logit, newx = X_test, s = "lambda.1se", type = "response"))
auc_yes(y_test, p_test_lasso)

# Compare to a baseline glm (no regularization)
glm_base <- glm(default ~ student + balance + income, data = train, family = binomial())
p_test_glm <- predict(glm_base, newdata = test, type = "response")
auc_yes(y_test, p_test_glm)

# ============================================================
# 4) Beyond linearity — polynomials + splines (logistic)
# ============================================================
# We'll model default ~ balance using nonlinear terms and tune the "knob" with CV.

# Create CV folds ON THE TRAINING SET ONLY
set.seed(5352)
folds <- vfold_cv(train, v = 10, strata = default)

# 4A) Tune polynomial degree for balance
degrees <- 1:6
cv_auc_poly <- rep(NA_real_, length(degrees))

for (i in seq_along(degrees)) {
  d <- degrees[i]
  aucs <- c()

  for (fold in fold_ids(folds)) {
    sp <- folds$splits[[fold]]
    tr <- analysis(sp)
    va <- assessment(sp)

    fit <- glm(default ~ poly(balance, degree = d, raw = TRUE),
               data = tr, family = binomial())

    p <- predict(fit, newdata = va, type = "response")
    aucs <- c(aucs, auc_yes(va$default, p))
  }

  cv_auc_poly[i] <- mean(aucs)
}

plot(degrees, cv_auc_poly, type = "b",
     xlab = "Polynomial degree",
     ylab = "10-fold CV AUC (train only)")

best_deg <- degrees[which.max(cv_auc_poly)]
best_deg

# Fit best polynomial on full training set; evaluate once on test
fit_poly <- glm(default ~ poly(balance, degree = best_deg, raw = TRUE),
                data = train, family = binomial())
p_test_poly <- predict(fit_poly, newdata = test, type = "response")
auc_yes(y_test, p_test_poly)

# 4B) Tune spline df for balance
dfs <- c(3, 4, 5, 6, 8, 10)
cv_auc_spline <- rep(NA_real_, length(dfs))

for (i in seq_along(dfs)) {
  df_i <- dfs[i]
  aucs <- c()

  for (fold in fold_ids(folds)) {
    sp <- folds$splits[[fold]]
    tr <- analysis(sp)
    va <- assessment(sp)

    fit <- glm(default ~ bs(balance, df = df_i),
               data = tr, family = binomial())

    p <- predict(fit, newdata = va, type = "response")
    aucs <- c(aucs, auc_yes(va$default, p))
  }

  cv_auc_spline[i] <- mean(aucs)
}

plot(dfs, cv_auc_spline, type = "b",
     xlab = "Spline degrees of freedom",
     ylab = "10-fold CV AUC (train only)")

best_df <- dfs[which.max(cv_auc_spline)]
best_df

fit_spline <- glm(default ~ bs(balance, df = best_df),
                  data = train, family = binomial())
p_test_spline <- predict(fit_spline, newdata = test, type = "response")
auc_yes(y_test, p_test_spline)

# Optional: visualize fitted probability vs balance
# (We plot on a grid of balance values for interpretation.)
bal_grid <- data.frame(balance = seq(min(df$balance), max(df$balance), length.out = 200))

bal_grid$prob_glm    <- predict(glm(default ~ balance, data = train, family = binomial()),
                                newdata = bal_grid, type = "response")
bal_grid$prob_poly   <- predict(fit_poly,   newdata = bal_grid, type = "response")
bal_grid$prob_spline <- predict(fit_spline, newdata = bal_grid, type = "response")

plot(bal_grid$balance, bal_grid$prob_glm, type = "l",
     xlab = "Balance", ylab = "P(default = Yes)")
lines(bal_grid$balance, bal_grid$prob_poly)
lines(bal_grid$balance, bal_grid$prob_spline)
legend("topleft", legend = c("Linear logit", paste0("Poly d=", best_deg), paste0("Spline df=", best_df)),
       lty = 1, bty = "n")

# ============================================================
# 5) Optional: GAM (skip if you want)
# ============================================================
# If you want a one-liner demo:
# library(mgcv)
# gam_fit <- gam(default ~ s(balance) + s(income) + student, data = train, family = binomial())
# p_test_gam <- predict(gam_fit, newdata = test, type = "response")
# auc_yes(y_test, p_test_gam)
# plot(gam_fit, pages = 1)

# ============================================================
# 6) Bridge to the credit scoring project (no data needed here)
# ============================================================
# Talking points for the walkthrough:
# - Lock your split ONCE and save it (or save indices).
# - Create CV folds ON TRAIN ONLY.
# - Baseline GLM -> then try:
#     * ridge/lasso via glmnet
#     * nonlinear terms via poly()/bs()
#     * simple feature engineering
# - Compare using the SAME folds/metrics.
# - Touch the test set once at the end.

# End of demo.
