library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)


# Step 1: Define base folder
crime_folder_2024 <- "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Obtained-Data/Crime datas/9dbe69340116121c0bdf844daaf0ba79077cf22a"

# Step 2: Get all CSV files recursively
crime_files_2024 <- list.files(path = crime_folder_2024, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Step 3: Read and combine them into one dataframe
crime_data_2024 <- map_df(crime_files_2024, read_csv)

# Step 4: View the structure
glimpse(crime_data_2024)

crime_filtered_2024 <- crime_data_2024 %>%
  filter(`Crime type` %in% c("Drugs", "Vehicle crime", "Robbery"))

table(crime_filtered_2024$`Crime type`)

crime_filtered_2024 <- crime_filtered_2024 %>%
  mutate(
    Month = ym(Month),                            # Converts "2024-01" to Date
    District = word(`LSOA name`, 1)               # First word of LSOA name
  )

# Group and count crimes per district per month per type
crime_summary_2024 <- crime_filtered_2024 %>%
  group_by(District, Month, `Crime type`) %>%
  summarise(Crime_Count = n(), .groups = "drop")

# Save the cleaned summary file
write_csv(crime_summary_2024,
          "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_2024.csv")
