##### -------------------------------------------------------------------------------------------
##### Fitting a Binomial LASSO Model with glmnet
##### -------------------------------------------------------------------------------------------
# Load libraries:
library(glmnet)

# Encode data in correct format:
encode_binomial <- function(df) {
  new_df <- df[rep(seq_len(nrow(df)), df$farm_size), ]
  result_vector <- integer(0)
  for (i in 1:nrow(df)) {
    num_ones <- round(df$target[i] * df$farm_size[i])
    current_vector <- c(rep(1, num_ones), rep(0, df$farm_size[i] - num_ones))
    result_vector <- c(result_vector, current_vector)
  }
  new_df$target <- result_vector
  new_df
}

# Fit a binomial LASSO model:
fit_binomial <- function(df) {
  excluded_vars <- c("target", "farm_size")
  train <- encode_binomial(df)
  X <- train[, !colnames(train) %in% excluded_vars]
  X$target <- train$target
  X <- model.matrix(target ~ . - 1, data = X)
  y <- train$target
  mod <- cv.glmnet(X, y, family = "binomial")
  #nonzero <- names(which(coef(mod, s = "lambda.1se")[-1, ] != 0))
  #X_sparse <- X[, which(colnames(X) %in% nonzero)]
  #train_sparse <- cbind("target" = y, as.data.frame(X_sparse))
  #glm(target ~ ., data = train_sparse, family  = "binomial")
  mod
}

## Hint: to get predictions with the model procuced by fit_binomial(), a test data.frame called "test" needs to be transformed:
# excluded_vars <- c("target", "farm_size")
# X_test <- test[, !colnames(test) %in% excluded_vars]
# X_test$target <- test$target
# X_test <- model.matrix(target ~ . - 1, data = X_test)
## Predictions are then created by:
# pred <- predict(mod, s = mod$lambda.1se, newx = X_test, type = "response")

