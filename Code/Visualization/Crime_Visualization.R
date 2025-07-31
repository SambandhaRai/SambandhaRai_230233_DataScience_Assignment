library(tidyverse)

Crime = read_csv("Cleaned Data/cleanedCrime.csv")
Crime <- Crime %>%
  mutate(Month = as.integer(Month))
colnames(Crime)
glimpse(Crime)
View(Crime)

LSOA = read_csv("Cleaned Data/cleanedPostcode_LSOA.csv")
colnames(LSOA)
glimpse(LSOA)
View(LSOA)

# --- Boxplot for Drug Offence rate in the district for both counties --- #
DrugCrime = Crime %>%
  filter(`Crime type` == "Drugs") %>%
  rename(Year = Year)

# Aggregate drug crimes per District, County, and Year
DrugCrime_Agg = DrugCrime %>%
  group_by(County, District, Year) %>%
  summarise(Total_Drug_Crime = sum(Crime_Count), .groups = "drop")

# Pivot longer LSOA population so we can match by year
Population_Long = LSOA %>%
  pivot_longer(
    cols = starts_with("Population"),
    names_to = "Year",
    names_prefix = "Population",
    values_to = "Population"
  ) %>%
  mutate(Year = as.numeric(Year))  # Ensure year is numeric

# Aggregate average population per District per Year
Population_Agg = Population_Long %>%
  group_by(County, District, Year) %>%
  summarise(Avg_Pop = mean(Population, na.rm = TRUE), .groups = "drop")

# Join population with crime data
DrugCrime_with_Rate = DrugCrime_Agg %>%
  left_join(Population_Agg, by = c("County", "District", "Year")) %>%
  filter(!is.na(Avg_Pop), Avg_Pop > 0) %>%
  mutate(Drug_Offense_Rate = (Total_Drug_Crime / Avg_Pop) * 1000)

# Plot South Yorkshire
DrugCrime_with_Rate %>%
  filter(County == "SOUTH YORKSHIRE") %>%
  ggplot(aes(x = District, y = Drug_Offense_Rate)) +
  geom_boxplot(fill = "#0571b0", alpha = 0.7, outlier.color = "red") +
  labs(title = "Drug Offense Rate per District in South Yorkshire (2022–2025)",
       x = "District", y = "Offense Rate per 1000 people") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot West Yorkshire
DrugCrime_with_Rate %>%
  filter(County == "WEST YORKSHIRE") %>%
  ggplot(aes(x = District, y = Drug_Offense_Rate)) +
  geom_boxplot(fill = "#ca0020", alpha = 0.7, outlier.color = "red") +
  labs(title = "Drug Offense Rate per District in West Yorkshire (2022–2025)",
       x = "District", y = "Offense Rate per 1000 people") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================================ #

# ---  Radar chart for Vehicle crime rate for any one of the two counties --- #
library(fmsb) # for radar chart

# Filter for Vehicle crime in 2023-02 and West Yorkshire
vehicle_crime_data = Crime %>%
  filter(`Crime type` == "Vehicle crime", Year == 2023, 
         Month == 2, County == "WEST YORKSHIRE") %>%
  count(District, name = "Count")

# Prepare data for radar chart (fmsb package requires max/min rows)
max_val = max(vehicle_crime_data$Count)
radar_df = vehicle_crime_data %>%
  column_to_rownames("District") %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame()

# Add required max and min rows at the top
radar_df = rbind(rep(max_val, ncol(radar_df)), rep(0, ncol(radar_df)), radar_df)

# Plot the radar chart
radarchart(radar_df,
           axistype = 1,
           pcol = "blue",
           pfcol = rgb(0.2, 0.5, 0.8, 0.5),
           plwd = 2,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "black",
           caxislabels = seq(0, max_val, length.out = 5),
           title = "Vehicle Crime Rate by District (West Yorkshire, Feb 2023)")

# ============================================================================ #

# --- Pie chart for Robbery rate for any one of two counties --- #
Robbery_data <- Crime %>%
  filter(
    `Crime type` == "Robbery", 
    County == "SOUTH YORKSHIRE", 
    Year == 2023, 
    Month == 5
  )

# Join on LSOA code instead of shortPostcode
Robbery_with_pop <- Robbery_data %>%
  left_join(LSOA %>% select(`LSOA code`, District, Population2023), by = "LSOA code") %>%
  filter(!is.na(Population2023))

# Handle duplicate district columns if any
if ("District.x" %in% colnames(Robbery_with_pop)) {
  Robbery_with_pop <- Robbery_with_pop %>%
    mutate(District = coalesce(District.x, District.y)) %>%
    select(-District.x, -District.y)
}

# Now group by District and summarize
Robbery_rate_district <- Robbery_with_pop %>%
  group_by(District) %>%
  summarise(
    total_robbery = sum(Crime_Count),
    total_pop = sum(Population2023)
  ) %>%
  mutate(rate_per_1000 = (total_robbery / total_pop) * 1000)

# Plot pie chart
ggplot(Robbery_rate_district, aes(x = "", y = rate_per_1000, fill = District)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Robbery Rate (May 2023) - South Yorkshire") +
  theme_void()

# ============================================================================ #

# --- Line chart for Drug offense rates per 10,000 people --- #

DrugCrimes <- Crime %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(County, Year) %>%
  summarise(Total_Drug_Crime = sum(Crime_Count), .groups = "drop")

# 2. Prepare population long format and aggregate by County and Year
Population_Long <- LSOA %>%
  pivot_longer(
    cols = starts_with("Population"),
    names_to = "Year",
    names_prefix = "Population",
    values_to = "Population"
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(County, Year) %>%
  summarise(Total_Pop = sum(Population, na.rm = TRUE), .groups = "drop")

# 3. Join drug crime data with population data
DrugCrime_with_Pop <- DrugCrimes %>%
  left_join(Population_Long, by = c("County", "Year")) %>%
  filter(!is.na(Total_Pop), Total_Pop > 0) %>%
  mutate(Drug_Offense_Rate_per_10000 = (Total_Drug_Crime / Total_Pop) * 10000)

# 4. Plot line chart
ggplot(DrugCrime_with_Pop, aes(x = Year, y = Drug_Offense_Rate_per_10000, color = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(DrugCrime_with_Pop$Year)) +
  labs(
    title = "Drug Offense Rate per 10,000 People by County (All Years)",
    x = "Year",
    y = "Drug Offense Rate per 10,000 People",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ============================================================================ #