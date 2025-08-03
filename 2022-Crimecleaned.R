library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)

# Step 1: Read all CSVs
crime_folder <- "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Obtained-Data/Crime datas/c446b58b0e0d9b0051c50917a155fe290d3b7e78"
crime_files <- list.files(path = crime_folder, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Read all files
crime_data <- map_df(crime_files, read_csv)

# See what we have
glimpse(crime_data)


unique(crime_data$Month)

crime_data <- crime_data %>%
  mutate(Month = ym(Month)) 

# Filter for only the 3 relevant crime types
crime_filtered <- crime_data %>%
  filter(`Crime type` %in% c("Drugs", "Vehicle crime", "Robbery"))


crime_filtered <- crime_filtered %>%
  mutate(District = word(`LSOA name`, 1))


crime_summary <- crime_filtered %>%
  group_by(District, Month, `Crime type`) %>%
  summarise(Crime_Count = n(), .groups = "drop")


write_csv(crime_summary,
          "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_2022.csv")

