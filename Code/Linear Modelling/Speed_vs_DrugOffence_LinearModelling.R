library(tidyverse)
library(dplyr)
library(ggplot2)

# --- Average Download speed vs Drug Offense Rate per 10000 people --- #
BroadbandSpeed = read_csv("Cleaned Data/cleanedBroadband.csv")
Crime = read_csv("Cleaned Data/cleanedCrime.csv")
LSOA = read_csv("Cleaned Data/cleanedPostcode_LSOA.csv")

# 2. Map LSOA to shortPostcode in crime data
crime_postcode <- Crime %>%
  left_join(LSOA %>% select(`LSOA code`, shortPostcode), by = "LSOA code") %>%
  filter(!is.na(shortPostcode))

# 3. Filter for Drug offenses in 2023, aggregate by shortPostcode
drug_crimes_postcode <- crime_postcode %>%
  filter(Year == 2023, `Crime type` == "Drugs") %>%
  count(shortPostcode, name = "DrugOffenseCount")

# 4. Get population and county info for each postcode
postcode_info <- LSOA %>%
  select(shortPostcode, Population2023, County) %>%
  distinct()

# 5. Calculate drug offense rate per 10,000 people
crime_with_rate <- drug_crimes_postcode %>%
  left_join(postcode_info, by = "shortPostcode") %>%
  filter(!is.na(Population2023)) %>%
  mutate(DrugOffenseRate = (DrugOffenseCount / Population2023) * 10000) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# 6. Aggregate broadband data by postcode and county
broadband_filtered <- BroadbandSpeed %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(shortPostcode, County) %>%
  summarise(avgDownload = mean(avgDownload, na.rm = TRUE), .groups = "drop")

# 7. Merge broadband and crime rate data
merged_data <- inner_join(broadband_filtered, crime_with_rate, by = c("shortPostcode", "County"))

# 8. Plot: Drug Offense Rate vs Average Download Speed
ggplot(merged_data, aes(x = DrugOffenseRate, y = avgDownload, color = County)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Download Speed vs Drug Offense Rate per 10,000 People (2023)",
    x = "Drug Offense Rate per 10,000 People",
    y = "Average Download Speed (Mbps)",
    color = "County"
  ) +
  theme_minimal()

# 9. Linear model with county interaction
lm_model <- lm(avgDownload ~ DrugOffenseRate * County, data = merged_data)
cat("----- Linear Model Summary -----\n")
print(summary(lm_model))

# 10. Overall correlation
overall_cor <- cor(merged_data$avgDownload, merged_data$DrugOffenseRate, use = "complete.obs")
cat("\nOverall Correlation Coefficient:", round(overall_cor, 3), "\n")

# 11. Correlation by county
county_correlations <- merged_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(avgDownload, DrugOffenseRate), .groups = "drop")

cat("\nCorrelation by County:\n")
print(county_correlations)
