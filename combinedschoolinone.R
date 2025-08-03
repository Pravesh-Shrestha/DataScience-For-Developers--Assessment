library(tidyverse)

# Load all three years
school_21 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/school_2021-22_cleaned.csv")
school_22 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/school_2022-23_cleaned.csv")
school_23 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/school_2023-24_cleaned.csv")

# Combine all into one dataframe
combined_school <- bind_rows(school_21, school_22, school_23)

# Save the combined data
write_csv(combined_school, 
          "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/combined_school-data.csv")
