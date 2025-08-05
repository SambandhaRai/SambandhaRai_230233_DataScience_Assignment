library(tidyverse)
library(dplyr)
library(scales)
library(forcats)
library(ggplot2)

HousePrices <- read_csv("Cleaned Data/cleanedHousePrices.csv")
Broadband <- read_csv("Cleaned Data/cleanedBroadband.csv")
Crime <- read_csv("Cleaned Data/cleanedCrime.csv")
School <- read_csv("Cleaned Data/cleanedSchool.csv")
Town_cleaned <- read_csv("Cleaned Data/Towns.csv")

# === 1. House Prices Summary ===
house_prices_summary <- HousePrices %>%
  group_by(County, District, Town) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop")

# === 2. Broadband Speeds ===
broadband_summary <- Broadband %>%
  group_by(County, District, Town) %>%
  summarise(AvgDownload = mean(avgDownload, na.rm = TRUE), .groups = "drop")

# === 3. Crime Summary (Group by District) ===
district_crime <- Crime %>%
  group_by(County, District) %>%
  summarise(TotalCrimeCount = sum(Crime_Count, na.rm = TRUE), .groups = "drop")

# Map District to Town
district_to_town <- Town_cleaned %>% select(District, Town) %>% distinct()

crime_summary <- district_crime %>%
  left_join(district_to_town, by = "District") %>%
  filter(!is.na(Town)) %>%
  group_by(County, District, Town) %>%
  summarise(TotalCrimeCount = mean(TotalCrimeCount, na.rm = TRUE), .groups = "drop")

# === 4. School Performance (2023-2024 only) ===
school_summary <- School %>%
  filter(Year == "School(2023-2024)", !ATT8SCR %in% c("NE", "SUPP", "NA")) %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%
  group_by(County, District, Town) %>%
  summarise(AvgAttainment = mean(ATT8SCR, na.rm = TRUE), .groups = "drop")

# === 5. Merge All Datasets ===
merged_all <- house_prices_summary %>%
  left_join(broadband_summary, by = c("County", "District", "Town")) %>%
  left_join(crime_summary, by = c("County", "District", "Town")) %>%
  left_join(school_summary, by = c("County", "District", "Town"))

# === 6. Handle Missing Values ===
merged_all <- merged_all %>%
  mutate(
    AvgPrice = coalesce(AvgPrice, mean(AvgPrice, na.rm = TRUE)),
    AvgDownload = coalesce(AvgDownload, mean(AvgDownload, na.rm = TRUE)),
    TotalCrimeCount = coalesce(TotalCrimeCount, mean(TotalCrimeCount, na.rm = TRUE)),
    AvgAttainment = coalesce(AvgAttainment, mean(AvgAttainment, na.rm = TRUE))
  )

# === 7. Normalize & Score ===
norm_data <- merged_all %>%
  mutate(
    norm_price = rescale(-AvgPrice),
    norm_crime = rescale(-TotalCrimeCount),
    norm_attainment = rescale(AvgAttainment),
    norm_download = rescale(AvgDownload),
    QualityScore = 0.4 * norm_price + 0.3 * norm_crime + 0.2 * norm_attainment + 0.1 * norm_download
  ) %>%
  arrange(desc(QualityScore))

# === 8. Extract Top 10 Towns ===
top_10_towns <- norm_data %>%
  slice(1:10) %>%
  mutate(
    TownLabel = paste0(Town, " (", District, ")"),
    TownLabel = fct_reorder(TownLabel, QualityScore),
    Rating = round(QualityScore * 10, 1)
  )
View(top_10_towns)

# === 9. Print Results ===
cat("\nTop 10 Towns to Live In:\n")
print(top_10_towns %>% select(County, District, Town, QualityScore, Rating))

cat("\nTop 10 Fastest Internet:\n")
print(merged_all %>% arrange(desc(AvgDownload)) %>% slice(1:10) %>% select(County, District, Town, AvgDownload))

cat("\nTop 10 Most Affordable Towns:\n")
print(merged_all %>% arrange(AvgPrice) %>% slice(1:10) %>% select(County, District, Town, AvgPrice))

cat("\nTop 10 Safest Towns:\n")
print(merged_all %>% arrange(TotalCrimeCount) %>% slice(1:10) %>% select(County, District, Town, TotalCrimeCount))

cat("\nTop 10 Best Schools:\n")
print(merged_all %>% arrange(desc(AvgAttainment)) %>% slice(1:10) %>% select(County, District, Town, AvgAttainment))

# === 10. Visualize ===
ggplot(top_10_towns, aes(x = reorder(TownLabel, Rating), y = Rating, fill = County)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  labs(
    title = "Top 10 Recommended Towns",
    x = "Town (District)",
    y = "Rating [0–10]"
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  scale_fill_manual(values = c(
    "SOUTH YORKSHIRE" = "darkred",
    "WEST YORKSHIRE" = "navy"
  )) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11)
  )
