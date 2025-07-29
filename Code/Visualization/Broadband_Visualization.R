library(tidyverse)
library(ggplot2)

Broadband = read_csv("Cleaned Data/cleanedBroadband.csv")
colnames(Broadband)
glimpse(Broadband)
View(Broadband)

# --- Boxplots for average download speed for both counties (District vs Speed(Mbps)) --- # 

west_yorkshire = Broadband %>% 
  filter(County == "WEST YORKSHIRE")
south_yorkshire = Broadband %>% 
  filter(County == "SOUTH YORKSHIRE")

# Plot for West Yorkshire
plot_west = ggplot(west_yorkshire, aes(x = District, y = avgDownload)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Average Download Speed by District in West Yorkshire",
       x = "District", y = "Average Download Speed (Mbps)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for South Yorkshire
plot_south = ggplot(south_yorkshire, aes(x = District, y = avgDownload)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red") +
  labs(title = "Average Download Speed by District in South Yorkshire",
       x = "District", y = "Average Download Speed (Mbps)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_west)
print(plot_south)

# ======================================================================================= #


# --- Barchart for both counties (Two barcharts) download speeds (variable Town vs Speed) --- #

broadband_town_avg = Broadband %>%
  group_by(County, Town) %>%
  summarise(meanDownload = mean(avgDownload, na.rm = TRUE)) %>%
  ungroup()

west_yorkshire_town = broadband_town_avg %>% 
  filter(County == "WEST YORKSHIRE")
south_yorkshire_town = broadband_town_avg %>% 
  filter(County == "SOUTH YORKSHIRE")

# Bar chart for West Yorkshire
chart_west <- ggplot(west_yorkshire_town, aes(x = reorder(Town, meanDownload), y = meanDownload)) +
  geom_col(fill = "skyblue") +
  coord_flip() +  
  labs(title = "Average Download Speed by Town - West Yorkshire",
       x = "Town", y = "Average Download Speed (Mbps)") +
  theme_minimal()

# Bar chart for South Yorkshire
chart_south <- ggplot(south_yorkshire_town, aes(x = reorder(Town, meanDownload), y = meanDownload)) +
  geom_col(fill = "lightgreen") +
  coord_flip() +
  labs(title = "Average Download Speed by Town - South Yorkshire",
       x = "Town", y = "Average Download Speed (Mbps)") +
  theme_minimal()

print(chart_west)
print(chart_south)
