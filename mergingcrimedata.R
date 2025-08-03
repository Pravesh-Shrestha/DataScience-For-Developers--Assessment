# Load libraries
library(readr)
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)

# Step 1: Read all yearly cleaned CSVs
crime_2022 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_2022.csv")
crime_2023 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_2023.csv")
crime_2024 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_2024.csv")

# Add Year column
crime_2022$Year <- 2022
crime_2023$Year <- 2023
crime_2024$Year <- 2024

# Combine all
crime_all <- bind_rows(crime_2022, crime_2023, crime_2024)

# Step 2: Define valid districts
valid_districts <- c(
  "Barnsley", "Rotherham", "Sheffield", "Doncaster",       # South Yorkshire
  "Leeds", "Bradford", "Kirklees", "Wakefield", "Calderdale"  # West Yorkshire
)

# Step 3: Clean and filter
crime_clean <- crime_all %>%
  filter(!is.na(District)) %>%                   # Remove NAs
  mutate(District = str_trim(District)) %>%
  filter(District %in% valid_districts)          # Keep only valid ones

# Step 4: Recalculate summary if needed
crime_summary <- crime_clean %>%
  group_by(District, Year, `Crime type`) %>%
  summarise(Crime_Count = sum(Crime_Count), .groups = "drop")

# Step 5: Save cleaned dataset
write_csv(crime_clean, "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_total.csv")
write_csv(crime_summary, "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary.csv")
