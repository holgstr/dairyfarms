##### -------------------------------------------------------------------------------------------
##### Fitting a Random Forest Using mlr3 and ranger
##### -------------------------------------------------------------------------------------------
fit_forest <- function(df) {
  task <- as_task_regr(df, target = "target")
  learner = lrn("regr.ranger")
  learner$train(task)
  learner
}
