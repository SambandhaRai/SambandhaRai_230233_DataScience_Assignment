library(tidyverse)
library(dplyr)

# --- House Price vs Download Speed for both Counties in single diagram --- #
# --- (include linear model summary report and correlation) --------------- #
county <- c("SOUTH YORKSHIRE", "WEST YORKSHIRE")

HousePrices = read_csv("Cleaned Data/cleanedHousePrices.csv")

BroadbandSpeed = read_csv("Cleaned Data/cleanedBroadband.csv")

# Compute average house price per postcode and county
avg_house_price = HousePrices %>%
  filter(County %in% county) %>%
  group_by(shortPostcode, County) %>%
  summarise(mean_price = mean(Price, na.rm = TRUE), .groups = "drop")

# Compute average download speed per postcode and county
avg_download_speed = BroadbandSpeed %>%
  filter(County %in% county) %>%
  group_by(shortPostcode, County) %>%
  summarise(mean_download = mean(avgDownload, na.rm = TRUE), .groups = "drop")

# Merge datasets on short postcode and county
combined_data = left_join(avg_house_price, avg_download_speed, by = c("shortPostcode", "County")) %>%
  drop_na()

# Visualize the relationship with regression lines per county
combined_data %>%
  ggplot(aes(x = mean_download, y = mean_price, color = County)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_log10() +
  labs(
    title = "Average House Price vs Download Speed",
    x = "Mean Download Speed (Mbps)",
    y = "Mean House Price (£)"
  ) +
  theme_minimal(base_size = 15) 

# For SOUTH YORKSHIRE
south_data = filter(combined_data, County == "SOUTH YORKSHIRE")
south_lm = lm(mean_price ~ mean_download, data = south_data)
cat("\nModel summary for SOUTH YORKSHIRE:\n")
print(summary(south_lm))
cat("Correlation:", round(cor(south_data$mean_price, south_data$mean_download), 3), "\n")

# For WEST YORKSHIRE
west_data = filter(combined_data, County == "WEST YORKSHIRE")
west_lm = lm(mean_price ~ mean_download, data = west_data)
cat("\nModel summary for WEST YORKSHIRE:\n")
print(summary(west_lm))
cat("Correlation:", round(cor(west_data$mean_price, west_data$mean_download), 3), "\n")
