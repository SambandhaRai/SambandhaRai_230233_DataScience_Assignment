library(tidyverse)

Town_cleaned = read_csv("Cleaned Data/Towns.csv")
Postcode_LSOA = read_csv("Obtained Data/Postcode/Postcode to LSOA.csv")

Postcode_LSOA = Postcode_LSOA %>%
  select(lsoa11cd, pcds)

# Match using starts with manually (slow)
matched_data = Town_cleaned %>%
  rowwise() %>%
  mutate(
    lsoa11cd = Postcode_LSOA$lsoa11cd[which(startsWith(Postcode_LSOA$pcds, shortPostcode))[1]]
  ) %>%
  ungroup() %>%
  filter(!is.na(lsoa11cd)) %>%
  select(`LSOA code` = lsoa11cd, shortPostcode, Town, District, County, Population2020, Population2021,
         Population2022, Population2023, Population2024)

# View and save output
View(matched_data)

write_csv(matched_data, "Cleaned Data/cleanedPostcode_LSOA.csv")
