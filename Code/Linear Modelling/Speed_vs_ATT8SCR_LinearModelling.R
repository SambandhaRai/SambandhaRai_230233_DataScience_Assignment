library(tidyverse)
library(ggpubr)

# Load data
Broadband = read_csv("Cleaned Data/cleanedBroadband.csv", show_col_types = FALSE)
School = read_csv("Cleaned Data/cleanedSchool.csv", show_col_types = FALSE)

# Clean and convert columns
School = School %>%
  mutate(
    Year = str_extract(Year, "\\d{4}"),
    Year = as.integer(Year),
    ATT8SCR = as.numeric(ATT8SCR)  # Make sure ATT8SCR is numeric
  )

School %>% count(County)

# Get latest year
latest_year = School %>%
  filter(!is.na(Year)) %>%
  distinct(Year) %>%
  arrange(desc(Year)) %>%
  slice(1) %>%
  pull(Year)

cat("Latest Year found:", latest_year, "\n")

# Filter and group school data
ks4_latest = School %>%
  filter(Year == latest_year) %>%
  group_by(shortPostcode, County) %>%
  summarise(ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = "drop")

# Join with broadband
merged_data = inner_join(Broadband, ks4_latest, by = c("shortPostcode", "County"))

cat("Merged data dimensions:", dim(merged_data), "\n")

# Plot
Net_Vs_Score = ggplot(merged_data, aes(x = ATT8SCR, y = avgDownload, color = County)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = paste("Average Download Speed vs Attainment 8 Score (", latest_year, ")", sep = ""),
    x = "Attainment 8 Score",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal()

print(Net_Vs_Score)

# Step 6: Linear model with interaction between ATT8SCR and County
model = lm(avgDownload ~ ATT8SCR * County, data = merged_data)
cat("Linear model summary:\n")
print(summary(model))

# Step 7: Calculate correlation coefficients
overall_cor = cor(merged_data$avgDownload, merged_data$ATT8SCR, use = "complete.obs")
cat("Overall Correlation Coefficient:", round(overall_cor, 3), "\n")

cor_by_county = merged_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(avgDownload, ATT8SCR, use = "complete.obs")) %>%
  arrange(desc(Correlation))

cat("Correlation by County:\n")
print(cor_by_county)
