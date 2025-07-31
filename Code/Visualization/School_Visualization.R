library(tidyverse)
library(ggplot2)

School = read_csv("Cleaned Data/cleanedSchool.csv")
colnames(School)
glimpse(School)
View(School)

# --- Boxplot for Average attainment 8 score 2022 – South Yorkshire --- #
south_school <- School %>%
  filter(County == "SOUTH YORKSHIRE") %>%
  filter(Year == "School(2021-2022)") %>%
  filter(!ATT8SCR %in% c("NE", "SUPP")) %>%       # remove non-numeric entries
  mutate(ATT8SCR = as.numeric(ATT8SCR))           # convert to numeric

# Attainment 8 Score by District
ggplot(south_school, aes(x = District, y = ATT8SCR)) +
  geom_boxplot(fill = "#1f77b4", color = "black") +
  labs(title = "Boxplot of Average Attainment 8 Scores (2022)",
       subtitle = "South Yorkshire",
       x = "District",
       y = "Attainment 8 Score") +
  theme_minimal()

# ============================================================================ #

# --- Boxplot for Average attainment 8 score 2022 – West Yorkshire --- #
west_school <- School %>%
  filter(County == "WEST YORKSHIRE") %>%
  filter(Year == "School(2021-2022)") %>%
  filter(!ATT8SCR %in% c("NE", "SUPP")) %>%       # remove non-numeric entries
  mutate(ATT8SCR = as.numeric(ATT8SCR))           # convert to numeric

ggplot(west_school, aes(x = District, y = ATT8SCR)) +
  geom_boxplot(fill = "#1f77b4", color = "black") +
  labs(title = "Boxplot of Average Attainment 8 Scores (2022)",
       subtitle = "West Yorkshire",
       x = "District",
       y = "Attainment 8 Score") +
  theme_minimal()

# ============================================================================ #

# --- Line Graph to show the relationship between attainment 8 score and years over #
# --- multiple districts in South and West Yorkshire ------------------------------ #
attainment_trend <- School %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  filter(!ATT8SCR %in% c("NE", "SUPP")) %>%
  mutate(
    ATT8SCR = as.numeric(ATT8SCR),
    County = str_to_title(County),
    Year = str_extract(Year, "\\d{4}-\\d{4}"),
    District = str_to_title(District),
    Label = paste(District, "(", County, ")")
  )

# Extract unique labels for each county
south_labels <- unique(attainment_trend$Label[attainment_trend$County == "South Yorkshire"])
west_labels  <- unique(attainment_trend$Label[attainment_trend$County == "West Yorkshire"])

# Manually assign light shades for South Yorkshire and dark shades for West Yorkshire
custom_colors <- c(
  setNames(c("#A8DADC", "#FFE66D", "#C3FDB8", "#F7C59F"), south_labels), # Light shades
  setNames(c("#457B9D", "#A47551", "#6B8E23", "#7B5EA7", "#9E7676"), west_labels) # Dark shades
)

# Plot
ggplot(attainment_trend, aes(x = Year, y = ATT8SCR, group = Label, color = Label)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.1) +
  stat_summary(fun = mean, geom = "point", size = 2.5) +
  facet_wrap(~ County, scales = "free_x") +
  labs(
    title = "Attainment 8 Score Trend Across Years",
    x = "Year",
    y = "Average Attainment 8 Score",
    color = "District (County)"
  ) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )
