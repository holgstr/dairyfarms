
## Pre-processing and Imputation

# source("imputation.R")
source("preprocessing.R")

hf_oc <- dfs$hf_oc
hf_uc <- dfs$hf_uc
sim_oc <- dfs$sim_oc
sim_uc <- dfs$sim_uc

hf_oc_target <- hf_oc %>%
  select(target) %>%
  mutate(race = "HF")

sim_oc_target <- sim_oc %>%
  select(target) %>%
  mutate(race = "SIM")

df_target <- data.frame(
  Column1 = numeric(),
  Column2 = character(),
  stringsAsFactors = FALSE
)

list_of_dfs <- list(
  oc = data.frame(
    Overconditioned = numeric(), 
    Race = character(),
    density =  numeric(),
    stringsAsFactors = FALSE),
  uc = data.frame(
    Underconditioned = numeric(), 
    Race = character(),
    density =  numeric(),
    stringsAsFactors = FALSE))

target <- data.frame(
    target = numeric(), 
    Race = character(),
    type =  character(),
    stringsAsFactors = FALSE)

for (i in seq_along(dfs)) {
  df <- dfs[[i]]
  df_target <- as.data.frame(df[, "target"])
  df_name <- names(dfs[i])
  df_target <- mutate(df_target, Race = toupper(strsplit(df_name, "_")[[1]][1]),
                      type = toupper(strsplit(df_name, "_")[[1]][2]))
  colnames(df_target)[1] <- "target"
  target <- rbind(target, df_target)
}

means <- target %>%
  group_by(type, Race) %>%
  summarize(mean_target = mean(target))%>%
  mutate(position = ifelse(type == "OC", 30, 35))

showzero <- subset(target, target == 0)
showzero$target <- 0.01


# Modelling procedure zero Inflation Plot:

ggplot(target, aes(x = target, fill = Race)) +
  geom_histogram(binwidth = 0.02, alpha = 1, color = "black", boundary = 0) +
  geom_bar_pattern(data = showzero,
                   width = 0.02, 
                   fill = "lightgrey", 
                   color = "black", 
                   linetype = "dashed",
                   pattern_angle = 50, 
                   pattern_density = 0.01,
                   alpha = 0.5) +
  scale_pattern_manual(values = "stripe") +
  scale_fill_manual(values = c("orange", "salmon")) +
  geom_vline(data = means, aes(xintercept = mean_target), color = "black", linetype = "dashed",linewidth = 0.7) +
  geom_text(data = means, aes(x = mean_target + 0.05, y = position, label = round(mean_target, 2)), 
            color = "black", size = 10, vjust = -0.5) +
  facet_grid(type ~ Race, scales = "free_y") + 
  scale_x_continuous(expand = c(0, 0.08)) +
  guides(pattern = "none") +
  labs(x = "Percentage of Farms", y = "Count", fill = "Farm breed") + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        axis.text = element_text(size = 40, color = "black"),
        axis.title = element_text(size = 40, color = "black"),
        panel.grid.major = element_line(color = "#E8E8E8", linewidth = 0.5),
        panel.grid.minor = element_line(color = "#E8E8E8", linewidth = 0.5),
        legend.text = element_text(size = 38),
        legend.title = element_text(size = 40),
        strip.text = element_text(size = 40),
        panel.spacing.x = unit(1.5, "lines"))
