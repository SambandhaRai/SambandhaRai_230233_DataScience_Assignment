library(tidyverse)

# === Setup ===
base_path = "Obtained Data/School"
year_folders = c("School(2021-2022)", "School(2022-2023)", "School(2023-2024)")

# Valid postcode prefixes in Yorkshire
yorkshire_prefixes = c("BD", "DN", "HD", "HG", "HU", "HX", "LS", "S", "WF", "YO")

# === Load and Clean Town Data ===
towns = read_csv("Cleaned Data/Towns.csv", show_col_types = FALSE) %>%
  mutate(
    shortPostcode = str_to_upper(str_trim(shortPostcode)),
    County = str_to_upper(str_trim(County))
  )

towns_yorkshire = towns %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# === Function to Read and Clean Each Year's School Data ===
read_clean_ks4final = function(year_folder) {
  folder_path = file.path(base_path, year_folder)
  
  # Search recursively inside folder_path for CSV files matching pattern
  csv_file = list.files(folder_path, pattern = "(?i)england_ks4final.*\\.csv$", full.names = TRUE, recursive = TRUE)
  
  if (length(csv_file) == 0) {
    message(paste("No CSV found in", folder_path))
    return(tibble())
  }
  
  df = read_csv(csv_file[1], show_col_types = FALSE)  # read first matching file
  
  # Pick only relevant columns present in the data
  cols_to_keep = c("LEA", "URN", "SCHNAME", "TOWN", "PCODE", "ATT8SCR")
  cols_to_keep = intersect(cols_to_keep, colnames(df))
  
  if (length(cols_to_keep) == 0) {
    message(paste("No relevant columns found in", csv_file[1]))
    return(tibble())
  }
  
  df_clean = df %>%
    select(all_of(cols_to_keep)) %>%
    na.omit() %>%
    mutate(
      shortPostcode = str_extract(PCODE, "^[A-Z0-9]+") %>% str_to_upper() %>% str_trim(),
      Year = year_folder
    ) %>%
    filter(str_sub(shortPostcode, 1, 2) %in% yorkshire_prefixes)
  
  return(df_clean)
}

# === Read All Years' Data ===
ks4_filtered = map_dfr(year_folders, read_clean_ks4final)

# === Merge with Town Data and Final Filter ===
combined_ks4final = ks4_filtered %>%
  left_join(towns_yorkshire, by = "shortPostcode") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  select(
    LEA, URN, SCHNAME, PCODE, ATT8SCR, shortPostcode, Year,
    Town, District, County
  )

# === Output ===
if (nrow(combined_ks4final) > 0) {
  View(combined_ks4final)
} else {
  message("No data found for South or West Yorkshire after filtering and join.")
}

write_csv(combined_ks4final, "Cleaned Data/cleanedSchool.csv")
