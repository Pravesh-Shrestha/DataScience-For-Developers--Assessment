library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)


# Step 1: Define base folder
crime_folder_2023 <- "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Obtained-Data/Crime datas/4e1e27d9ca1f571bb31c715bfff0e572cc1fd636"

# Step 2: Get all CSV files recursively
crime_files_2023 <- list.files(path = crime_folder_2023, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Step 3: Read and combine them into one dataframe
crime_data_2023 <- map_df(crime_files_2023, read_csv)

# Step 4: View the structure
glimpse(crime_data_2023)

crime_filtered_2023 <- crime_data_2023 %>%
  filter(`Crime type` %in% c("Drugs", "Vehicle crime", "Robbery"))

table(crime_filtered_2023$`Crime type`)

crime_filtered_2023 <- crime_filtered_2023 %>%
  mutate(
    Month = ym(Month),                            # Converts "2023-01" to Date
    District = word(`LSOA name`, 1)               # First word of LSOA name
  )

# Group and count crimes per district per month per type
crime_summary_2023 <- crime_filtered_2023 %>%
  group_by(District, Month, `Crime type`) %>%
  summarise(Crime_Count = n(), .groups = "drop")

# Save the cleaned summary file
write_csv(crime_summary_2023,
          "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_2023.csv")
