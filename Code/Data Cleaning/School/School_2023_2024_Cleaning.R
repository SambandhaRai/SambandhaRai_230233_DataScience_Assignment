library(tidyverse)
library(dplyr)
library(readr)

School_2023_2024_ks4_final_raw = read_csv("Obtained Data/School/School(2023-2024)/2023-2024/england_ks4final.csv")
View(School_2023_2024_ks4_final_raw)
colnames(School_2023_2024_ks4_final_raw)

School_2023_2024_school_info_raw = read_csv("Obtained Data/School/School(2023-2024)/2023-2024/england_school_information.csv")
View(School_2023_2024_school_info_raw)
colnames(School_2023_2024_school_info_raw)

# Find the right score column name
colnames(School_2023_2024_ks4_final_raw)

# Clean and extract URN + Attainment 8 Score
School_2023_2024_ks4_final_filtered <- School_2023_2024_ks4_final_raw %>%
  select(URN, ATT8SCR) %>%  # replace ATT8SCR if it's different
  mutate(ATT8SCR = as.numeric(ATT8SCR)) %>%
  filter(!is.na(ATT8SCR))
View(School_2023_2024_ks4_final_filtered)

# Create a county column based on LANAME
School_2023_2024_school_info_filtered <- School_2023_2024_school_info_raw %>%
  mutate(
    County = case_when(
      LANAME %in% c("Barnsley", "Doncaster", "Rotherham", "Sheffield") ~ "South Yorkshire",
      LANAME %in% c("Leeds", "Bradford", "Wakefield", "Calderdale", "Kirklees") ~ "West Yorkshire",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(County)) %>%  # remove schools outside West or South Yorkshire
  filter(ISSECONDARY == 1)    # only secondary schools
View(School_2023_2024_school_info_filtered)

School_2023_2024_school_info_filtered <- School_2023_2024_school_info_filtered %>%
  select(URN, SCHNAME, LANAME, County, TOWN, POSTCODE, AGELOW, AGEHIGH)
View(School_2023_2024_school_info_filtered)

# Combining
School_2023_2024_combined <- School_2023_2024_school_info_filtered %>%
  left_join(School_2023_2024_ks4_final_filtered, by = "URN") %>%
  filter(!is.na(ATT8SCR))  # Keep schools with valid scores
View(School_2023_2024_combined)

# Storing
write.csv(School_2023_2024_combined, "Cleaned Data/School Cleaned/School(2023-2024)_Cleaned.csv")
