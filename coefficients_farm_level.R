##### -------------------------------------------------------------------------------------------
##### Coefficients for the Binomial and Beta Models after Variable Selection using farm-level Predictors
##### -------------------------------------------------------------------------------------------
# Coefficients of the Binomial Models
library(glmnet)
library(gamlss)
source("preprocessing.R")
source("fit_binomial.R")

variable_farm <- colnames(dfs$hf_oc)[c(3:11, 14:25, 26:29, 31:44)]
variable_cow <- colnames(dfs$hf_oc)[c(1:2, 12, 30)]

dfs_farm <- lapply(dfs, function(df) {
  selected_vars <- intersect(names(df), c(variable_farm, "target"))
  df[,selected_vars]
})

set.seed(1234)
models <- lapply(dfs_farm, fit_binomial)
coefs_raw <- lapply(models, function(x) {coef(x, exact = TRUE)})
coefs <- lapply(coefs_raw, function(x) {vec <- as.numeric(x)
                                        vec[vec == 0] <- NA
                                        vec})
coefs_hf <- data.frame("variable" = rownames(coefs_raw[[3]]),
                       "UC" = coefs[[3]],
                       "OC" = coefs[[4]])
coefs_sim <- data.frame("variable" = rownames(coefs_raw[[1]]),
                        "UC" = coefs[[1]],
                        "OC" = coefs[[2]])

# Coefficients for the Beta Models
##### We need run this code below for all 4 problems:
beta_mod_hfoc <- gamlss(target ~ ., family = BEZI(mu.link = "logit"), data = dfs_farm$hf_oc, trace = FALSE)
beta_mod_hfoc2 <- stepGAIC(beta_mod_hfoc, trace = FALSE)
s1 <- summary(beta_mod_hfoc2)
coefs_hf_oc <- data.frame("variable" = rownames(s1[-c(13, 14), ]),
                       "OC" = as.numeric(s1[-c(13, 14), 1]),
                       "P_OC" = as.numeric(s1[-c(13, 14), 4]))

beta_mod_hfuc <- gamlss(target ~ ., family = BEZI(mu.link = "logit"), data = dfs_farm$hf_uc, trace = FALSE)
beta_mod_hfuc2 <- stepGAIC(beta_mod_hfuc, trace = FALSE)
s2 <- summary(beta_mod_hfuc2)
coefs_hf_uc <- data.frame("variable" = rownames(s2[-c(15, 16), ]),
                          "UC" = as.numeric(s2[-c(15, 16), 1]),
                          "P_UC" = as.numeric(s2[-c(15, 16), 4]))
coefs_hf_beta <- na.omit(merge(coefs_hf_oc, coefs_hf_uc,by = "variable" ,all = TRUE))


beta_mod_simoc <- gamlss(target ~ ., family = BEZI(mu.link = "logit"), data = dfs_farm$sim_oc, trace = FALSE)
beta_mod_simoc2 <- stepGAIC(beta_mod_simoc, trace = FALSE)
s3 <- summary(beta_mod_simoc2)
coefs_sim_oc <- data.frame("variable" = rownames(s3[-c(20, 21), ]),
                          "OC" = as.numeric(s3[-c(20, 21), 1]),
                          "P_OC" = as.numeric(s3[-c(20, 21), 4]))

beta_mod_simuc <- gamlss(target ~ ., family = BEZI(mu.link = "logit"), data = dfs_farm$sim_uc, trace = FALSE)
beta_mod_simuc2 <- stepGAIC(beta_mod_simuc, trace = FALSE)
s4 <- summary(beta_mod_simuc2)
coefs_sim_uc <- data.frame("variable" = rownames(s4[-c(23, 24), ]),
                          "UC" = as.numeric(s4[-c(23, 24), 1]),
                          "P_UC" = as.numeric(s4[-c(23, 24), 4]))
coefs_sim_beta <- na.omit(merge(coefs_sim_oc, coefs_sim_uc,by = "variable" ,all = TRUE))
