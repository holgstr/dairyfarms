
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Over- and Underconditioning on German Dairy Farms

<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

This repository contains the R code used to conduct analyses and create
visualizations for the consulting project on over- and underconditioning
on German dairy farms.

**Structure:**

- The study data must be saved as `raw.csv` in the `data` folder to
  replicate the results.
- The `plots` folder contains the code and image files for the
  visualizations created for the final report.
- `preprocessing.R` contains the code for the pre-processing that is
  necessary for further analysis.
- `coefficients.R` and `coefficients_farm_level.R` contain the code for
  model fitting and parameter estimates for the interpretable
  statistical models (using either all predictors or farm-level
  predictors only).
- `cv.R` and `cv_farm_level.R` contain the code for the cross-validation
  results for the empirical performance analysis for the variable
  selection and modeling frameworks we compare (using either all
  predictors or farm-level predictors only).
- `Feature_Imp.R` contains the code for the Permutation Feature
  Importance plots for the random forest model (using farm-level
  predictors only).
- `fit_binomial.R` and `fit_forest.R` contain the code for helper
  functions for fitting LASSO binomial and random forest models,
  respectively.

**Main Libraries:**

- `glmnet` for fitting LASSO-regularized binomial GLMs
- `gamlss` for fitting zero-inflated beta regression models and
  conducting stepwise selection
- `mlr3verse` for fitting a random forest with the `ranger` package and
  other tools.
