# Disease_Cases_Analysis_R_Project

# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# ---------------------------
# 📁 Sample Dataset: Patients by Disease & Region
# ---------------------------
disease_data <- data.frame(
  Region = c("North", "South", "East", "West", "Central", "North", "South", "East", "West", "Central"),
  Influenza = c(120, 130, 140, 110, 125, 150, 135, 145, 115, 130),
  Diabetes = c(60, 75, 70, 65, 80, 85, 72, 68, 76, 79),
  Hypertension = c(90, 100, 95, 105, 110, 115, 102, 108, 98, 99)
)

# ---------------------------
# 📊 Summary Statistics
# ---------------------------
summary_stats <- data.frame(
  Disease = c("Influenza", "Diabetes", "Hypertension"),
  Mean = sapply(disease_data[2:4], mean),
  Median = sapply(disease_data[2:4], median),
  Std_Dev = sapply(disease_data[2:4], sd)
)

cat("\n📊 Summary Statistics:\n")
print(summary_stats)

# ---------------------------
# 📉 Visualizations
# ---------------------------

# Convert to long format using tidyr
long_disease <- disease_data %>%
  pivot_longer(cols = -Region, names_to = "Disease", values_to = "Patients")

# Boxplot
ggplot(long_disease, aes(x = Disease, y = Patients, fill = Disease)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Boxplot of Patient Counts by Disease")

# Histogram
ggplot(long_disease, aes(x = Patients, fill = Disease)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  facet_wrap(~Disease) +
  theme_minimal() +
  ggtitle("Distribution of Patients by Disease")

# Bar Plot - Region-wise Patient Count
ggplot(long_disease, aes(x = Region, y = Patients, fill = Disease)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  ggtitle("Disease Prevalence by Region") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------------------
# 🏆 Bonus: Region with Highest & Lowest Cases per Disease
# ---------------------------

get_top_bottom <- function(disease) {
  top <- disease_data[which.max(disease_data[[disease]]), c("Region", disease)]
  bottom <- disease_data[which.min(disease_data[[disease]]), c("Region", disease)]
  return(list(Top_Region = top, Lowest_Region = bottom))
}

cat("\n🏆 Disease Hotspots:\n")
print(list(
  Influenza = get_top_bottom("Influenza"),
  Diabetes = get_top_bottom("Diabetes"),
  Hypertension = get_top_bottom("Hypertension")
))

# ---------------------------
# 💡 Insights
# ---------------------------
cat("\n💡 Insights:\n")
cat("- Influenza has the highest number of patients and most variation across regions.\n")
cat("- Hypertension is the most consistent and widespread chronic condition.\n")
cat("- The North region has high cases of Influenza and Hypertension.\n")
cat("- Diabetes shows moderate prevalence across all regions, with Central showing the highest cases.\n")
