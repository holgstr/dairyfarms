##### -------------------------------------------------------------------------------------------
##### CV Performance using all Predictors
##### -------------------------------------------------------------------------------------------
# Load library
library(gamlss)
library(glmnet)
library(caret)
library(rsample)
library(mlr3verse)

# Load data for preprocessing and imputation and model
# source("imputation.R")
source("preprocessing.R")
source("fit_binomial.R")
source("fit_forest.R")

# Define data set names
dataset_names <- c("sim_uc", "sim_oc", "hf_uc", "hf_oc")

# Handler for low-prevalence levels in CV:
adjust_levels <- function(test, train) {
  for (col in names(test)) {
    if (is.factor(train[[col]])) {
      zero_categories <- names(which(table(train[[col]]) == 0))
      test[[col]][test[[col]] %in% zero_categories] <- names(which.max(table(train[[col]])))[1]
    }
  }
  return(test)
}

# For each data set, perform 10-fold CV:
for (i in seq_len(length(dfs))) {
  # Select the dataset to analyze
  df <- dfs[[i]]
  set.seed(1234)
  folds <- createFolds(df$target, k = 10)
  all_binomial_pred <- numeric()
  all_binomial_null_pred <- numeric()
  all_binomial_unpenalized_pred <- numeric()
  all_binomial_AIC_pred <- numeric()
  all_beta_normal_pred <- numeric()
  all_beta_pred <- numeric()
  all_beta_null_pred <- numeric()
  all_forest_pred <- numeric()
  all_actual <- numeric()
  all_nonzeros_bin <- numeric()
  all_nonzeros_bin_AIC <- numeric()
  all_nonzeros_beta <- numeric()


  for (j in 1:10) {
    test_indices <- folds[[j]]
    train_indices <- setdiff(seq_len(nrow(df)), test_indices)
    train <- df[train_indices, ]
    test <- df[test_indices, ]

    # Fit binomial model
    # Null model:
    binomial_null <- suppressWarnings(glm(target ~ 1, family = binomial, weights = train$farm_size, data = train))
    binomial_null_pred <- predict(binomial_null, newdata = test, type = "response")
    # Unpenalized:
    train_unpenalized <- train[ , !names(train) %in% "farm_size"]
    binomial_unpenalized <- suppressWarnings(glm(target~., family = binomial, weights = train$farm_size, data = train_unpenalized))
    test_adapted <- adjust_levels(test, train)
    binomial_unpenalized_pred <- predict(binomial_unpenalized, newdata = test_adapted, type = "response")
    # Penalized:
    binomial_mod <- fit_binomial(train)
    excluded_vars <- c("target", "farm_size")
    X_test_binomial <- test[, !colnames(test) %in% excluded_vars]
    X_test_binomial$target <- test$target
    X_test_binomial <- model.matrix(target ~ . - 1, data = X_test_binomial)
    binomial_pred <- predict(binomial_mod, s = binomial_mod$lambda.1se, newx = X_test_binomial, type = "response")
    nonzero_bin <- sum(as.numeric(coef(binomial_mod)) != 0) - 1
    # AIC:
    binomial_AIC <- suppressWarnings(stepAIC(binomial_unpenalized, trace = FALSE))
    model_terms_bin <- terms(binomial_AIC)
    variable_names_bin <- attr(model_terms_bin, "term.labels")
    excluded_vars_bin <- c("target")
    binomial_AIC_pred <- predict(binomial_AIC, newdata = test, type = "response")
    nonzero_bin_AIC <- sum(as.numeric(coef(binomial_AIC)) != 0) - 1

    # Fit beta model
    # Null model:
    beta_null <- gamlss(target ~ 1, family = BEZI(mu.link = "logit"), data = train, trace = FALSE)
    beta_null_pred <- predict(beta_null, newdata = test, type = "response")
    # No selection:
    beta_mod_normal <- gamlss(target ~ ., family = BEZI(mu.link = "logit"), data = train, trace = FALSE)
    beta_normal_pred <- predict(beta_mod_normal, newdata = test_adapted, type = "response")
    # With Selection:
    beta_mod <- stepGAIC(beta_mod_normal, trace = FALSE)
    model_terms <- terms(beta_mod)
    variable_names <- attr(model_terms, "term.labels")
    excluded_vars <- c("target")
    beta_pred <- predict(beta_mod, newdata = test, type = "response")
    nonzero_beta <- sum(as.numeric(coef(beta_mod)) != 0) - 1

    # Fit random forest
    forest_mod <- fit_forest(train)
    forest_pred <- predict(forest_mod, newdata = test)

    # Accumulate predictions
    all_binomial_pred <- c(all_binomial_pred, binomial_pred)
    all_binomial_null_pred <- c(all_binomial_null_pred, binomial_null_pred)
    all_binomial_unpenalized_pred <- c(all_binomial_unpenalized_pred, binomial_unpenalized_pred)
    all_binomial_AIC_pred <- c(all_binomial_AIC_pred, binomial_AIC_pred)
    all_beta_normal_pred <- c(all_beta_normal_pred, beta_normal_pred)
    all_beta_null_pred <- c(all_beta_null_pred, beta_null_pred)
    all_beta_pred <- c(all_beta_pred, beta_pred)
    all_forest_pred <- c(all_forest_pred, forest_pred)

    # Accumulate non-zeroes
    all_nonzeros_bin <- c(all_nonzeros_bin, nonzero_bin)
    all_nonzeros_bin_AIC <- c(all_nonzeros_bin_AIC, nonzero_bin_AIC)
    all_nonzeros_beta <- c(all_nonzeros_beta, nonzero_beta)

    # Accumulate actual values
    all_actual <- c(all_actual, test$target)
  }

  # Calculate R squared
  binomial_r2 <- 1 - sum((all_actual - all_binomial_pred)^2) / sum((all_actual - mean(all_actual))^2)
  binomial_null_r2 <- 1 - sum((all_actual - all_binomial_null_pred)^2) / sum((all_actual - mean(all_actual))^2)
  binomial_unpenalized_r2 <- 1 - sum((all_actual - all_binomial_unpenalized_pred)^2) / sum((all_actual - mean(all_actual))^2)
  binomial_AIC_r2 <- 1 - sum((all_actual - all_binomial_AIC_pred)^2) / sum((all_actual - mean(all_actual))^2)
  beta_r2 <- 1 - sum((all_actual - all_beta_pred)^2) / sum((all_actual - mean(all_actual))^2)
  beta_null_r2 <- 1 - sum((all_actual - all_beta_null_pred)^2) / sum((all_actual - mean(all_actual))^2)
  beta_normal_r2 <- 1 - sum((all_actual - all_beta_normal_pred)^2) / sum((all_actual - mean(all_actual))^2)
  forest_r2 <- 1 - sum((all_actual - all_forest_pred)^2) / sum((all_actual - mean(all_actual))^2)

  # Results
  cat("Dataset", dataset_names[i], "Results:\n")
  cat("Beta R-squared:", beta_r2, "\n")
  cat("Beta Null R-squared:", beta_null_r2, "\n")
  cat("Beta Normal R-squared:", beta_normal_r2, "\n")
  cat("Binomial R-squared:", binomial_r2, "\n")
  cat("Binomial Null R-squared:", binomial_null_r2, "\n")
  cat("Binomial Normal R-squared:", binomial_unpenalized_r2, "\n")
  cat("Binomial AIC R-squared:", binomial_AIC_r2, "\n")
  cat("Random Forest R-squared:", forest_r2, "\n")
  cat("Parameters Selected Binomial:", mean(all_nonzeros_bin, na.rm = TRUE), "\n")
  cat("Parameters Selected Binomial with stepAIC:", mean(all_nonzeros_bin_AIC, na.rm = TRUE), "\n")
  cat("Parameters Selected Beta:", mean(all_nonzeros_beta, na.rm = TRUE), "\n\n")
}

#### Results
#Dataset sim_uc Results:
#Beta R-squared: -0.01696594
#Beta Null R-squared: -0.03459763
#Beta Normal R-squared: -0.2344259
#Binomial R-squared: 0.03731217
#Binomial Null R-squared: -0.01709089
#Binomial Normal R-squared: -0.2459711
#Binomial AIC R-squared: -0.1646621
#Random Forest R-squared: 0.03797673
#Parameters Selected Binomial: 4.1
#Parameters Selected Binomial with stepAIC: 27.9
#Parameters Selected Beta: 22.125

#Dataset sim_oc Results:
#Beta R-squared: -0.1458707
#Beta Null R-squared: -0.0121907
#Beta Normal R-squared: -0.501037
#Binomial R-squared: 0.03675678
#Binomial Null R-squared: -0.0294162
#Binomial Normal R-squared: -0.8350991
#Binomial AIC R-squared: -0.6799806
#Random Forest R-squared: 0.09783501
#Parameters Selected Binomial: 17.8
#Parameters Selected Binomial with stepAIC: 31.2
#Parameters Selected Beta: 17.2

#Dataset hf_uc Results:
#Beta R-squared: 0.1987682
#Beta Null R-squared: -0.0003477083
#Beta Normal R-squared: 0.1448179
#Binomial R-squared: 0.2133073
#Binomial Null R-squared: -0.1411767
#Binomial Normal R-squared: 0.07632722
#Binomial AIC R-squared: 0.0664312
#Random Forest R-squared: 0.2619607
#Parameters Selected Binomial: 28.3
#Parameters Selected Binomial with stepAIC: 48.2
#Parameters Selected Beta: 12.66667

#Dataset hf_oc Results:
#Beta R-squared: 0.5062138
#Beta Null R-squared: -0.004172
#Beta Normal R-squared: 0.5192862
#Binomial R-squared: 0.5451266
#Binomial Null R-squared: -0.299504
#Binomial Normal R-squared: 0.4947504
#Binomial AIC R-squared: 0.4921968
#Random Forest R-squared: 0.5581836
#Parameters Selected Binomial: 32.8
#Parameters Selected Binomial with stepAIC: 49.85714
#Parameters Selected Beta: 11.7
