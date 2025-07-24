library(tidyverse)
library(dplyr)

BroadbandSpeed_raw = read_csv("Obtained Data/Broadband Speed/201805_fixed_pc_performance_r03.csv")

BroadbandSpeed_clean = BroadbandSpeed_raw %>% 
  mutate(shortPostcode = str_trim(substring(postcode_space, 1,4))) %>%
  mutate(ID = row_number()) %>% 
  select("ID", "postcode area", "shortPostcode", "Average download speed (Mbit/s)",
         "Average upload speed (Mbit/s)", "Minimum download speed (Mbit/s)",
         "Minimum upload speed (Mbit/s)") %>% 
  drop_na() # removes rows with any N/A values

write_csv(BroadbandSpeed_clean, "Cleaned Data/cleanedBroadbandSpeed.csv")
