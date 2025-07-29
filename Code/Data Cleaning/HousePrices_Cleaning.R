library(tidyverse)

# Load and rename
years = 2021:2024
file_paths = paste0("Obtained Data/HousePrices/HousePrices", years, ".csv")
columns = c("TransactionID", "Price", "DateOfTransfer", "Postcode", "PropertyType",
             "OldOrNew", "Duration", "PAON", "SAON", "Street", "Locality",
             "Town", "District", "County", "PPDCategoryType", "RecordStatus")

data_list = lapply(file_paths, function(f) {
  df = read_csv(f, show_col_types = FALSE)
  names(df) = columns
  df
})

# Combine and filter
clean_house_prices = bind_rows(data_list) %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  mutate(
    shortPostcode = sub(" .*", "", Postcode),
    Year = substr(DateOfTransfer, 1, 4),
    Price = as.numeric(Price)
  ) %>%
  select(Postcode, shortPostcode, Year, PAON, Price, Town, District, County) %>%
  distinct() %>%
  drop_na()

View(clean_house_prices)

# Save result
write_csv(clean_house_prices, "Cleaned Data/cleanedHousePrices.csv")
