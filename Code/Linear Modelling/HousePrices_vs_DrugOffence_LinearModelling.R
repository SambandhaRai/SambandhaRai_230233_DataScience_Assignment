library(tidyverse)
library(ggpubr)

# --- Load datasets ---
HousePrices = read_csv("Cleaned Data/cleanedHousePrices.csv", show_col_types = FALSE)
Crime = read_csv("Cleaned Data/cleanedCrime.csv", show_col_types = FALSE)
Postcode = read_csv("Cleaned Data/cleanedPostcode_LSOA.csv", show_col_types = FALSE)

# --- Merge LSOA to crime data ---
crime_postcode_joined = Crime %>%
  left_join(Postcode %>% select(`LSOA code`, shortPostcode), by = c("LSOA code" = "LSOA code")) %>%
  filter(!is.na(shortPostcode))

# --- Filter drug-related crimes for 2023 ---
drug_crime_2023 = crime_postcode_joined %>%
  filter(Year == 2023, `Crime type` == "Drugs") %>%
  group_by(shortPostcode) %>%
  summarise(DrugCases = n(), .groups = "drop")

# --- Attach population and calculate offense rate ---
drug_crime_rate = drug_crime_2023 %>%
  left_join(Postcode %>% select(shortPostcode, Population2023, County) %>% distinct(), by = "shortPostcode") %>%
  mutate(OffenseRatePer10k = (DrugCases / Population2023) * 10000) %>%
  drop_na()

# --- Prepare house price data ---
avg_price_postcode = HousePrices %>%
  filter(Year == 2023, County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  group_by(shortPostcode, County) %>%
  summarise(MeanPrice = mean(Price, na.rm = TRUE), .groups = "drop")

# --- Combine with crime data ---
final_data = inner_join(avg_price_postcode, drug_crime_rate, by = c("shortPostcode", "County"))

# --- Scatterplot with regression ---
ggplot(final_data, aes(x = OffenseRatePer10k, y = MeanPrice, color = County)) +
  geom_point(size = 2.8, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "2023 House Prices vs Drug Offense Rates per 10,000 People",
    x = "Drug Offense Rate per 10,000 People",
    y = "Mean House Price (GBP)",
    color = "County"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_light()

# --- Run linear model with interaction term ---
model_result = lm(MeanPrice ~ OffenseRatePer10k * County, data = final_data)
summary(model_result)

# --- Correlation overall and by county ---
total_corr = cor(final_data$MeanPrice, final_data$OffenseRatePer10k, use = "complete.obs")
cat("Total Correlation:", round(total_corr, 3), "\n")

county_corrs = final_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(MeanPrice, OffenseRatePer10k))
print(county_corrs)
