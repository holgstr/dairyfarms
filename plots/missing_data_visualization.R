library(readxl)
library(ggplot2)
library(reshape2)


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


missing_df_plot <- subset(missing_df_long, variable %in% c("Missing_Count", "Missing_Percentage") & value > 0)


missing_count_plot <- subset(missing_df_plot, variable == "Missing_Count")
missing_percentage_plot <- subset(missing_df_plot, variable == "Missing_Percentage")


barplot_missing_count <- ggplot(missing_count_plot, aes(x = Column, y = value, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", linewidth = 1) +
  geom_text(aes(label = value), vjust = -0.3, size = 9, color = "black", position = position_dodge(0.9)) +
  theme_minimal() +
  labs(y = "Missing Count", x = NULL) +
  scale_fill_manual(values = c("orange", "salmon")) +
  scale_x_discrete(labels = c("Fasciola Detection", "Ostertagia Detection")) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
    legend.position = "none",
    axis.text.x = element_text(size = 13, color = "black"),
    axis.text.y = element_text(size = 7, color = "black"),
    axis.title = element_text(size = 24, color = "black"),
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_line(color = "gray", size = 0.5)
  ) +
  scale_y_continuous(breaks = seq(0, max(missing_count_plot$value, na.rm = TRUE), by = 1))



# Missing_Percentage Plot
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

ggsave("plots/barplot_missing_count.jpg", plot = barplot_missing_count, width = 18, height = 23, units = "cm")
ggsave("plots/barplot_missing_percentage.jpg", plot = barplot_missing_percentage, width = 18, height = 23, units = "cm")


print(barplot_missing_count)
print(barplot_missing_percentage)


# Breed grouped by region, education and size plot:
table(df$farm_breed, df$farm_region)
table(df$farm_breed, df$farm_education)
round(table(df$farm_breed, df$farm_education)/c(452, 73, 192)*100, 2)

df_size <- df[, c("farm_breed", "farm_size")]
df_size <- df_size[df_size$farm_breed %in% c("HF", "SIM"), ]
df_size$farm_size <- as.numeric(df_size$farm_size)
means <- aggregate(farm_size ~ farm_breed, data = df_size, mean)
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


# Breed grouped by size log plot:
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

