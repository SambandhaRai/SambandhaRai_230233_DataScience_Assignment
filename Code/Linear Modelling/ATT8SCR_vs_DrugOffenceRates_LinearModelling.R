library(tidyverse)
library(dplyr)
library(ggplot2)

# Load data
School = read_csv("Cleaned Data/cleanedSchool.csv")
Crime = read_csv("Cleaned Data/cleanedCrime.csv")
LSOA = read_csv("Cleaned Data/cleanedPostcode_LSOA.csv")

# --- Clean School Data --- #
School = School %>%
  mutate(
    Year = as.numeric(str_extract(Year, "\\d{4}")),
    ATT8SCR = as.numeric(na_if(ATT8SCR, "NE"))
  )

# Aggregate Attainment 8 score by postcode and year
att8_agg = School %>%
  group_by(shortPostcode, Year, County) %>%
  summarise(ATT8SCR = mean(ATT8SCR, na.rm = TRUE), .groups = "drop") %>%
  filter(Year == 2023, County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# --- Filter and Aggregate Drug Crime Data for 2023 --- #
crime_drugs_2023 = Crime %>%
  filter(`Crime type` == "Drugs", Year == 2023) %>%
  group_by(`LSOA code`, County, District, Year) %>%
  summarise(Drug_Count = sum(Crime_Count), .groups = "drop")

# Join with population and compute rate per 10,000
crime_rates = crime_drugs_2023 %>%
  left_join(LSOA, by = c("LSOA code", "County", "District")) %>%
  filter(!is.na(Population2023)) %>%
  mutate(Rate_per_10k = (Drug_Count / Population2023) * 10000) %>%
  group_by(shortPostcode, County) %>%
  summarise(Rate_per_10k = mean(Rate_per_10k, na.rm = TRUE), .groups = "drop") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# --- Merge School and Crime Data --- #
merged_df = att8_agg %>%
  inner_join(crime_rates, by = c("shortPostcode", "County"))

# --- Plot --- #
ggplot(merged_df, aes(x = ATT8SCR, y = Rate_per_10k, color = County)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Attainment 8 Score vs Drug Offense Rate per 10,000 (2023)",
    x = "Average Attainment 8 Score",
    y = "Drug Offenses per 10,000 People"
  ) +
  theme_minimal()

# --- Correlation Analysis --- #

# Overall correlation
cor_overall = cor(merged_df$ATT8SCR, merged_df$Rate_per_10k, use = "complete.obs")
cat("Overall Correlation:", round(cor_overall, 3), "\n")

# County-wise correlations
cor_sy = merged_df %>%
  filter(County == "SOUTH YORKSHIRE") %>%
  summarise(Correlation = cor(ATT8SCR, Rate_per_10k, use = "complete.obs")) %>%
  pull(Correlation)

cor_wy = merged_df %>%
  filter(County == "WEST YORKSHIRE") %>%
  summarise(Correlation = cor(ATT8SCR, Rate_per_10k, use = "complete.obs")) %>%
  pull(Correlation)

cat("South Yorkshire Correlation:", round(cor_sy, 3), "\n")
cat("West Yorkshire Correlation:", round(cor_wy, 3), "\n")
