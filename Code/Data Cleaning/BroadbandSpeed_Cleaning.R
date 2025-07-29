library(tidyverse)

# Load data
Broadband <- read_csv("Obtained Data/Broadband Speed/201805_fixed_pc_performance_r03.csv", show_col_types = FALSE)
HousePrices <- read_csv("Cleaned Data/cleanedHousePrices.csv", show_col_types = FALSE)

# Step 1: Make postcode map from HousePrices — get most common Town/District/County per short postcode
postcode_map <- HousePrices %>%
  mutate(shortPostcode = substr(Postcode, 1, 4)) %>%
  count(shortPostcode, Town, District, County, name = "freq") %>%
  group_by(shortPostcode) %>%
  slice_max(freq, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(shortPostcode, Town, District, County)

# Step 2: Clean Broadband data: extract short postcode by substring, keep relevant cols, remove NA
Broadband_clean <- Broadband %>%
  mutate(shortPostcode = substr(postcode_space, 1, 4)) %>%
  select(postcode, postcode_space, shortPostcode,
         avgDownload = `Average download speed (Mbit/s)`,
         avgUpload = `Average upload speed (Mbit/s)`,
         minDownload = `Minimum download speed (Mbit/s)`,
         minUpload = `Minimum upload speed (Mbit/s)`) %>%
  filter(!is.na(shortPostcode), !is.na(avgDownload)) %>%
  distinct()

# Step 3: Merge broadband with postcode map to get Town, District, County
Broadband_merged <- Broadband_clean %>%
  left_join(postcode_map, by = "shortPostcode") %>%
  filter(!is.na(District), !is.na(County))

# View or save
View(Broadband_merged)

write_csv(Broadband_merged, "Cleaned Data/cleanedBroadband.csv")
