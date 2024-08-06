library(readxl)
library(ggplot2)
library(reshape2)
library(ggpattern)

##############################################################################################
##############################################################################################
## Code for Barplot_breed:
##############################################################################################
##############################################################################################

# Load preprocessed data.
source("preprocessing.R")
# "df" is the general data we use for the introductory visualizations.

######### Dominanting Breed on Farms
barplot_breed <- ggplot(df, aes(x = farm_breed, fill = farm_breed)) +
  geom_bar(color = "black", linewidth = 1) +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.3, size = 9, color = "black") +
  theme_minimal() +
  labs(y = "Count", x = NULL) +
  scale_fill_manual(values = c("orange", "lightskyblue", "salmon")) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        legend.position = "none",
        axis.text = element_text(size = 24, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.minor = element_line(color = "gray", size = 0.5)) +
  scale_y_continuous(breaks = seq(0, 450, by = 100),
                     minor_breaks = seq(0, 450, by = 50))
# Save
ggsave("plots/barplot_breed.jpg", plot = barplot_breed, width = 15, height = 23, units = "cm")

#### Further info:
# Breed grouped by region:
table(df$farm_breed, df$farm_region)

##############################################################################################
##############################################################################################
## Code for Barplot_zero_Inflation:
##############################################################################################
##############################################################################################

## Pre-processing:
source("imputation.R")
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
  stringsAsFactors = FALSE)

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

## Modelling procedure zero Inflation Plot:

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
  labs(x = "Prevalence", y = "Count", fill = "Farm breed") + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        axis.text = element_text(size = 40, color = "black"),
        axis.title = element_text(size = 40, color = "black"),
        panel.grid.major = element_line(color = "#E8E8E8", linewidth = 0.5),
        panel.grid.minor = element_line(color = "#E8E8E8", linewidth = 0.5),
        legend.text = element_text(size = 38),
        legend.title = element_text(size = 40),
        strip.text = element_text(size = 40),
        panel.spacing.x = unit(1.5, "lines"))

##############################################################################################
##############################################################################################
## Code for Barplot_missing_percentage:
##############################################################################################
##############################################################################################

## Pre-processing:

SIM <- dfs$sim_uc
HF <- dfs$hf_uc
df_na <- list(SIM = SIM, HF = HF)

check_missing_df <- function(df) {
  total_count <- nrow(df)
  missing_count <- colSums(is.na(df))
  missing_percentage <- colMeans(is.na(df)) * 100
  missing_summary <- data.frame(
    Column = names(missing_count),
    Total_Count = total_count,
    Missing_Count = missing_count,
    Missing_Percentage = missing_percentage
  )
  return(missing_summary)
}

missing_summaries <- lapply(df_na, check_missing_df)

for (name in names(missing_summaries)) {
  cat("Missing Data Summary -", name, "\n")
  print(missing_summaries[[name]])
  cat("\n")
}

missing_df_long <- do.call(rbind, lapply(names(missing_summaries), function(name) {
  summary <- missing_summaries[[name]]
  summary$Dataset <- name
  return(summary)
}))
missing_df_long <- melt(missing_df_long, id.vars = c("Column", "Dataset"))
missing_percentage_plot <- subset(missing_df_plot, variable == "Missing_Percentage")

## Missing_Percentage Plot:
barplot_missing_percentage <- ggplot(missing_percentage_plot, aes(x = Column, y = value, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 1) +
  geom_text(aes(label = sprintf("%.1f%%", value)), vjust = -0.3, size = 9, color = "black", position = position_dodge(0.9)) +
  theme_minimal() +
  labs(y = "Missing Percentage", x = NULL, fill = "Farm Breed") +
  scale_fill_manual(values = c("orange", "salmon")) +
  scale_x_discrete(labels = c("Fasciola", "Ostertagia")) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        legend.position = "right",
        legend.title = element_text(size = 19, color = "black"),
        legend.text = element_text(size = 17, color = "black"),
        axis.text = element_text(size = 22, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.minor = element_line(color = "gray", size = 0.5)) +
  scale_y_continuous(breaks = seq(50, 450, by = 50))

ggsave("plots/barplot_missing_percentage.jpg", plot = barplot_missing_percentage, width = 18, height = 23, units = "cm")
print(barplot_missing_percentage)

##############################################################################################
##############################################################################################
## Code for Barplot_Farm_size_log and table for Breed grouped by region, education :
##############################################################################################
##############################################################################################

## Breed grouped by region, education table:
table(df$farm_breed, df$farm_region)
table(df$farm_breed, df$farm_education)
round(table(df$farm_breed, df$farm_education)/c(452, 73, 192)*100, 2)

df_size <- df[, c("farm_breed", "farm_size")]
df_size <- df_size[df_size$farm_breed %in% c("HF", "SIM"), ]
df_size$farm_size <- as.numeric(df_size$farm_size)
means <- aggregate(farm_size ~ farm_breed, data = df_size, mean)

## Breed grouped by size plot:
density_plot <- ggplot(df_size, aes(x = farm_size, fill = farm_breed)) + 
  geom_density(alpha = 0.5, color = "black", linewidth = 1) +
  geom_vline(data = means, aes(xintercept = farm_size, color = farm_breed), linetype = "dashed", linewidth = 1) +
  geom_text(data = means, aes(x = farm_size, y = 0.02, label = sprintf("%.1f", farm_size)), color = "black", size = 10, vjust = -0.5) +
  theme_minimal() +
  labs(y = "Density", x = "Farm Size", fill = "Farm Breed") +
  scale_fill_manual(values = c("orange", "salmon")) +
  scale_color_manual(values = c("orange", "salmon"), guide = FALSE) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        legend.position = "right",
        legend.title = element_text(size = 19, color = "black"),
        legend.text = element_text(size = 17, color = "black"),
        axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.minor = element_line(color = "gray", size = 0.5)) +
  scale_y_continuous(breaks = seq(0, 0.02, by = 0.005))
print(density_plot)

## Breed grouped by size log plot:
log_density_plot <- ggplot(df_size, aes(x = farm_size, fill = farm_breed)) + 
  geom_density(alpha = 0.5, color = "black", linewidth = 1) +
  geom_vline(data = means, aes(xintercept = farm_size, color = farm_breed), linetype = "dashed", linewidth = 1) +
  geom_text(data = means, aes(x = farm_size, y = 0.02, label = sprintf("%.1f", farm_size)), color = "black", size = 9, vjust = -0.5) +
  theme_minimal() +
  labs(y = "Density", x = "Farm Size", fill = "Farm Breed") +
  scale_fill_manual(values = c("orange", "salmon")) +
  scale_color_manual(values = c("orange", "salmon"), guide = FALSE) +
  scale_x_log10() + 
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
        legend.position = "right",
        legend.title = element_text(size = 22, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 24, color = "black"),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.minor = element_line(color = "gray", size = 0.5)) +
  scale_y_continuous(labels = scales::comma)
print(log_density_plot)

##############################################################################################
##############################################################################################
