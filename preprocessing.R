# Dplyr for pre-processing.
library(dplyr)

# Load data.
df <- read.csv("data/raw.csv")

# Missing Values also as category
df[is.na(df)] <- "unknown"

# Encode character features as factors.
df <- df %>% mutate_if(is.character, as.factor)

# Delete farm id.
df$farm_id <- NULL

# Recompute Primi-/Multiparous for some observations where proportions add to >100%.
df <- df %>%
  mutate(
    total = calv_primiparous + calv_multiparous,
    calv_primiparous = if_else(total > 1 & calv_primiparous == 1, 1 - calv_multiparous, calv_primiparous),
    calv_multiparous = if_else(total > 1 & calv_multiparous == 1, 1 - calv_primiparous, calv_multiparous)
  )
df$total <- NULL

# Delete variables that add up to 100% with another variable.
df$limb_not_lame_bb <- NULL
df$calv_primiparous <- NULL

# Delete proportion of normal conditioned cows.
df$cond_normal_bb <- NULL

# Separate Holstein (HF) and Fleckvieh (SIM).
sim_oc <- df %>% filter(farm_breed == "SIM")
hf_oc <- df %>% filter(farm_breed == "HF")

# Delete farm breed.
sim_oc$farm_breed <- NULL
hf_oc$farm_breed <- NULL

# Delete the farm not in South and farm region for SIM
sim_oc <- sim_oc[sim_oc$farm_region == "South", ] 
sim_oc$farm_region <- NULL

# Delete farm region South for HF
hf_oc <- hf_oc[hf_oc$farm_region != "South", ] 
hf_oc$farm_region <- droplevels(hf_oc$farm_region)

# Separate OC from UC into separate sets.
sim_uc <- sim_oc
sim_uc$cond_overconditioned_bb <- NULL
sim_oc$cond_underconditioned_bb <- NULL

hf_uc <- hf_oc
hf_uc$cond_overconditioned_bb <- NULL
hf_oc$cond_underconditioned_bb <- NULL

# Function to rename the target variable in a list of data frames to "target".
rename_target <- function(df) {
  # Find the column name starting with "cond"
  cond_col <- grep("^cond", names(df), value = TRUE)

  # If such a column is found, rename to "target"
  if (length(cond_col) > 0) {
    names(df)[names(df) == cond_col] <- "target"
  }

  df
}

# Apply the renaming function to each data frame in the list.
dfs <- lapply(list("sim_uc" = sim_uc, "sim_oc" = sim_oc, "hf_uc" = hf_uc, "hf_oc" = hf_oc), rename_target)

# As the result of this pre-processing, we have 4 data.frames in a list "dfs": sim_uc, sim_oc, hf_uc, hf_oc.
