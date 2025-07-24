library(tidyverse)
library(dplyr)

# Load South Yorkshire crime data
SouthYorkshireStreetCrime_raw = read_csv("Obtained Data/Crime/2025-04/2025-04-south-yorkshire-street.csv")
View(SouthYorkshireStreetCrime_raw)

# Clean South Yorkshire crime data
SouthYorkshireStreetCrime_cleaned = SouthYorkshireStreetCrime_raw %>%
  select(Location, `LSOA name`, `Crime type`) %>%
  filter(!is.na(Location), !is.na(`Crime type`)) %>%
  mutate(
    Location = na_if(str_replace(Location, "On or near ", ""), ""),
    District = word(`LSOA name`, 1),  # Extract district as first word of LSOA name
    County = "South Yorkshire"
  ) %>%
  drop_na(Location)
View(SouthYorkshireStreetCrime_cleaned)

# Load West Yorkshire crime data
WestYorkshireStreetCrime_raw = read_csv("Obtained Data/Crime/2025-04/2025-04-west-yorkshire-street.csv")
View(WestYorkshireStreetCrime_raw)

# Clean West Yorkshire crime data
WestYorkshireStreetCrime_cleaned = WestYorkshireStreetCrime_raw %>%
  select(Location, `LSOA name`, `Crime type`) %>%
  filter(!is.na(Location), !is.na(`Crime type`)) %>%
  mutate(
    Location = na_if(str_replace(Location, "On or near ", ""), ""),
    District = word(`LSOA name`, 1),  # Extract district as first word of LSOA name
    County = "West Yorkshire"
  ) %>%
  drop_na(Location)
View(WestYorkshireStreetCrime_cleaned)

# Combine both counties
Crime_combined = bind_rows(SouthYorkshireStreetCrime_cleaned, WestYorkshireStreetCrime_cleaned)
View(Crime_combined)

write.csv(Crime_combined, "Cleaned Data/cleanedCrime.csv", row.names = FALSE)
