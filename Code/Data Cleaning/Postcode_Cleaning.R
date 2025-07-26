library(tidyverse)
library(dplyr)

Town_cleaned = read_csv("Cleaned Data/Towns.csv")

Postcode_LSOA = read_csv("Obtained Data/Postcode/Postcode to LSOA.csv")

pattern = ' .*$'

Postcode_LSOA_Cleaned = Postcode_LSOA %>%
  select(lsoa11cd, pcds) %>% 
  mutate(shortPostcode = gsub(pattern,"",pcds)) %>% 
  right_join(Town_cleaned, by = "shortPostcode")  %>% 
  group_by(lsoa11cd) %>% 
  select(`LSOA code` = lsoa11cd, shortPostcode, Town, District, County) 

View(Postcode_LSOA_Cleaned)

write.csv(Postcode_LSOA_Cleaned, "Cleaned Data/cleanedPostcode_LSOA.csv")
