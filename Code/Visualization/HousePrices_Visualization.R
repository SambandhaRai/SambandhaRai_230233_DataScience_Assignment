library(tidyverse)
  
HousePrices = read_csv("Cleaned Data/cleanedHousePrices.csv")
colnames(HousePrices)
glimpse(HousePrices)
View(HousePrices)
  
# --- Line Graphs for Average house prices for both --- #
# --- counties in same diagram (Price and District) -------------------- #
Average_House_Price_Data <- HousePrices %>%
  group_by(County, Year) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), .groups = "drop")
  
# Plot: District vs Price, faceted by County (side by side)
ggplot(Average_House_Price_Data, aes(x = Year, y = AveragePrice, group = County, color = County)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average House Prices by County (2021–2024)",
    subtitle = "Side-by-side Line Graphs for West and South Yorkshire",
    x = "Year",
    y = "Average House Price (£)",
    color = "County"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

# ============================================================================ #

# --- Bar chart Average House Prices of 2023 for both counties --- #
hp_2023 <- HousePrices %>%
  filter(Year == 2023)

# Group by District and County, then calculate average price
avg_price_2023 <- hp_2023 %>%
  group_by(County) %>%
  summarise(AveragePrice = mean(Price, na.rm = TRUE), .groups = "drop")  

# Bar chart
ggplot(avg_price_2023, aes(x = County, y = AveragePrice, fill = County)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average House Prices by County in 2023",
    subtitle = "South and West Yorkshire",
    x = "County",
    y = "Average House Price (£)",
    fill = "County"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# ============================================================================ #

# --- Boxplot for average house prices for both counties --- #
ggplot(HousePrices, aes(x = District, y = Price)) +
  geom_boxplot(outlier.shape = 21, fill = "skyblue") +
  facet_wrap(~ County, scales = "free_x") +
  labs(
    title = "Average House Prices by District",
    x = "District",
    y = "House Price (£)"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================================ #