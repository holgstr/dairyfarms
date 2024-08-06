# Load preprocessed data.
source("preprocessing.R")
# "df" is the general data we use for the introductory visualizations.
library(ggplot2)

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
