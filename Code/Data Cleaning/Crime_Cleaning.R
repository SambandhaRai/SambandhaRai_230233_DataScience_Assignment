library(tidyverse)
library(data.table)
library(stringr)

# STEP 1: Load Postcode–LSOA mapping (South + West Yorkshire only)
postcode_lsoa <- read_csv("Cleaned Data/cleanedPostcode_LSOA.csv", show_col_types = FALSE) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  distinct(`LSOA code`, .keep_all = TRUE)

# STEP 2: Load and combine all raw crime CSVs
crime_files <- list.files("Obtained Data/Crime", pattern = "-street\\.csv$", recursive = TRUE, full.names = TRUE)

all_crimes <- map_df(crime_files, function(file) {
  # Extract Year and Month from file path (e.g., "2022-06/2022-06-south-yorkshire-street.csv")
  parts <- str_match(file, "(\\d{4})-(\\d{2})")
  year <- as.numeric(parts[2])
  month <- parts[3]
  
  read_csv(file, show_col_types = FALSE) %>%
    select(`Crime ID`, `LSOA code`, `Crime type`) %>%
    mutate(Year = year, Month = month)
})

# STEP 3: Summarize crime counts (including Month)
crime_summary <- all_crimes %>%
  filter(!is.na(`LSOA code`) & `LSOA code` != "") %>%
  group_by(`LSOA code`, Year, Month, `Crime type`) %>%
  summarise(Crime_Count = n(), .groups = "drop")

# STEP 4: Merge with postcode–LSOA data to get District and County
merged_crime <- crime_summary %>%
  left_join(postcode_lsoa, by = "LSOA code") %>%
  select(`LSOA code`, County, District, Year, Month, `Crime type`, Crime_Count)

# STEP 5: Remove unmatched rows
merged_cleaned <- merged_crime %>%
  filter(!is.na(County))

# STEP 6: View or save to CSV (includes Month now)
View(merged_cleaned)

write_csv(merged_cleaned, "Cleaned Data/cleanedCrime.csv")
