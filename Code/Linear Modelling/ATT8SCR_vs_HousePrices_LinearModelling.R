library(tidyverse)
library(ggplot2)

# --- Attainment 8 score vs House Price for both counties --- #
HousePrices = read_csv("Cleaned Data/cleanedHousePrices.csv", show_col_types = FALSE)
School = read_csv("Cleaned Data/cleanedSchool.csv", show_col_types = FALSE)

# Extract year as numeric (e.g., 2022 from "2022-2023")
School = School %>%
  mutate(Year = as.numeric(str_extract(Year, "\\d{4}")))

# Aggregate: average house price by postcode, year, and county
avg_price = HousePrices %>%
  group_by(shortPostcode, Year, County) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop")

# Aggregate: average Attainment 8 score by postcode and year
avg_att8 = School %>%
  filter(!ATT8SCR %in% c("SUPP", "NE")) %>%
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%
  group_by(shortPostcode, Year) %>%
  summarise(AvgAtt8 = mean(ATT8SCR, na.rm = TRUE), .groups = "drop")

# Merge datasets on shortPostcode and Year
merged_data = avg_price %>%
  inner_join(avg_att8, by = c("shortPostcode", "Year")) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# Remove extreme high-end house prices (top 5%)
filtered_data = merged_data %>%
  filter(AvgPrice <= quantile(AvgPrice, 0.95, na.rm = TRUE))

# Plot
ggplot(filtered_data, aes(x = AvgAtt8, y = AvgPrice, color = County)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_log10() +
  labs(
    title = "Relationship Between Attainment 8 Score and House Prices",
    x = "Average Attainment 8 Score",
    y = "Average House Price (log scale)"
  ) +
  theme_minimal()

# Linear model (log-price vs attainment)
lm_model = lm(log10(AvgPrice) ~ AvgAtt8, data = filtered_data)
summary(lm_model)

# Overall and County-wise Correlation: Attainment 8 vs House Price
filtered_data %>%
  group_by(County) %>%
  summarise(Correlation = cor(AvgAtt8, AvgPrice, use = "complete.obs")) %>%
  bind_rows(tibble(County = "Overall", Correlation = cor(filtered_data$AvgAtt8, filtered_data$AvgPrice, use = "complete.obs"))) %>%
  mutate(Correlation = round(Correlation, 3)) %>%
  print()
