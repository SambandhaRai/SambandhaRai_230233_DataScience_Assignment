library(tidyverse)
library(dplyr)

column_names = c(
  "TransactionID","Price", "Date", "Postcode", "PropertyType", "OldNew",
  "Duration", "PAON", "SAON", "Street", "Locality", "Town", "District","County",
  "PPDCategory", "RecordStatus"
)

# Cleaning each year's data
HousePrices2021 = read_csv("Obtained Data/HousePrices/HousePrices2021.csv")
colnames(HousePrices2021) = column_names

HousePrices2022 = read_csv("Obtained Data/HousePrices/HousePrices2022.csv")
colnames(HousePrices2022) = column_names

HousePrices2023 = read_csv("Obtained Data/HousePrices/HousePrices2023.csv")
colnames(HousePrices2023) = column_names

HousePrices2024 = read_csv("Obtained Data/HousePrices/HousePrices2024.csv")
colnames(HousePrices2024) = column_names

# Combining all cleaned data
HousePrices_combined = bind_rows(HousePrices2021, HousePrices2022, HousePrices2023, HousePrices2024)

dir.create("Cleaned Data", recursive = TRUE, showWarnings = FALSE)

write.csv(HousePrices_combined, "Cleaned Data/CombinedHousePrices.csv")

# Cleaned & Filtered data
cleanHousePrices = HousePrices_combined %>%
  filter(County=="SOUTH YORKSHIRE"|County=="WEST YORKSHIRE") %>% 
  mutate(shortPostcode = str_trim(substring(Postcode, 1,4))) %>% 
  mutate(Year=substring(Date,7,10)) %>% 
  arrange(County) %>% 
  select(Postcode,shortPostcode,Price,Year,PropertyType)

write.csv(cleanHousePrices, "Cleaned Data/cleanedHousePrices.csv", row.names = FALSE)
