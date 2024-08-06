##### -------------------------------------------------------------------------------------------
##### PFI values and Plots using farm-level predictors and a Random Forest
##### -------------------------------------------------------------------------------------------
# Load data for preprocessing and imputation
source("preprocessing.R")
source("fit_forest.R")

# Load libraries
library(mlr3verse)
library(iml)

# Seed
set.seed(12345)

# Farm-level covariates
variable_farm <- colnames(dfs$hf_oc)[c(3:11, 14:25, 26:29, 31:44)]

dfs_farm <- lapply(dfs, function(df) {
  selected_vars <- intersect(names(df), c(variable_farm, "target"))
  df[,selected_vars]
})

# Train models
models <- lapply(dfs_farm, function(x) {fit_forest(x)})

# Compute PFI function
compute_pfi <- function(df, model) {
  task <- as_task_regr(x = df, target = "target")
  data_x = task$data(cols = task$feature_names)
  data_y = task$data(cols = task$target_names)
  predictor = Predictor$new(model, data = data_x, y = data_y)
  FeatureImp$new(predictor, loss = "mse", compare = "ratio", n.repetitions = 20)
}

# Compute PFIs
pfi_sim_uc <- compute_pfi(dfs_farm[[1]], models[[1]])
pfi_sim_oc <- compute_pfi(dfs_farm[[2]], models[[2]])
pfi_hf_uc <- compute_pfi(dfs_farm[[3]], models[[3]])
pfi_hf_oc <- compute_pfi(dfs_farm[[4]], models[[4]])

# Get results
pfi_sim_uc2 <- pfi_sim_uc$clone(deep = TRUE)
pfi_sim_oc2 <- pfi_sim_oc$clone(deep = TRUE)
pfi_sim_uc2$results <- pfi_sim_uc2 $results[1:3,]
pfi_sim_uc2$results$feature <- c("Stabling System", "Farm Size", "Feed. Area per Animal")
pfi_sim_oc2$results <- pfi_sim_oc2$results[1:3,]
pfi_sim_oc2$results$feature <- c("Farm Size", "Stabling System", "Feed. Area per Animal")
pfi_hf_uc2 <- pfi_hf_uc$clone(deep = TRUE)
pfi_hf_oc2 <- pfi_hf_oc$clone(deep = TRUE)
pfi_hf_uc2$results <- pfi_hf_uc2$results[1:3,]
pfi_hf_uc2$results$feature <- c("Farm Size", "Farm Region", "Lying Area per Animal")
pfi_hf_oc2$results <- pfi_hf_oc2$results[1:3,]
pfi_hf_oc2$results$feature <- c("Farm Region", "Farm Size", "Feed. Area per Animal")

# Plots
p_sim_uc <- plot(pfi_sim_uc2) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        legend.position = "none",
        axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        plot.title = element_text(size = 24, color = "black"),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_line(color = "gray", size = 0.5)) +
  ggtitle("SIM-UC")

p_sim_oc <- plot(pfi_sim_oc2) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        legend.position = "none",
        axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        plot.title = element_text(size = 24, color = "black"),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_line(color = "gray", size = 0.5)) +
  ggtitle("SIM-OC")

p_hf_uc <- plot(pfi_hf_uc2) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        legend.position = "none",
        axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        plot.title = element_text(size = 24, color = "black"),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_line(color = "gray", size = 0.5)) +
  ggtitle("HF-UC")

p_hf_oc <- plot(pfi_hf_oc2) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        legend.position = "none",
        axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        plot.title = element_text(size = 24, color = "black"),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_line(color = "gray", size = 0.5)) +
  ggtitle("HF-OC")

library(gridExtra)
plot_imp <- grid.arrange(p_sim_uc, p_sim_oc, p_hf_uc, p_hf_oc, ncol = 2, nrow = 2, layout_matrix = rbind(c(1, 2), c(3, 4)))


# Save
ggsave("plots/importance.jpg", plot = plot_imp, width = 55, height = 26, units = "cm")
