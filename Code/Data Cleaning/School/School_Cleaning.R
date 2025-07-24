library(tidyverse)
library(dplyr)

# Reading Cleaned dataset
School_2021 = read_csv("Cleaned Data/School Cleaned/School(2021-2022)_Cleaned.csv")
School_2022 = read_csv("Cleaned Data/School Cleaned/School(2022-2023)_Cleaned.csv")
School_2023 = read_csv("Cleaned Data/School Cleaned/School(2023-2024)_Cleaned.csv")

# Adding Year in each
School_2021 = School_2021 %>% 
  mutate(Year = "2021-2022")
School_2022 = School_2022 %>% 
  mutate(Year = "2022-2023")
School_2023 = School_2023 %>% 
  mutate(Year = "2023-2024")

School_cleaned = bind_rows(School_2021, School_2022, School_2023)
View(School_cleaned)

# Store
write.csv(School_cleaned, "Cleaned Data/School Cleaned/cleanedSchool.csv")
