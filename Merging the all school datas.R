# Load libraries
library(tidyverse)
library(readr)

# STEP 1: Load all cleaned yearly CSVs
school_21 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/school_2021-22_cleaned.csv")
school_22 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/school_2022-23_cleaned.csv")
school_23 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/school_2023-24_cleaned.csv")

clean_school <- function(df) {
  df %>%
    mutate(
      `Attainment 8 Score` = as.character(`Attainment 8 Score`),
      `Attainment 8 Score` = ifelse(`Attainment 8 Score` %in% c("NE", "SUPP"), NA, `Attainment 8 Score`),
      `Attainment 8 Score` = as.numeric(`Attainment 8 Score`),
      
      `Progress 8 Score` = as.character(`Progress 8 Score`),
      `Progress 8 Score` = ifelse(`Progress 8 Score` %in% c("NE", "SUPP"), NA, `Progress 8 Score`),
      `Progress 8 Score` = as.numeric(`Progress 8 Score`),
      
      `Grade 5+ Eng & Math (%)` = as.character(`Grade 5+ Eng & Math (%)`),
      `Grade 5+ Eng & Math (%)` = str_replace(`Grade 5+ Eng & Math (%)`, "%", ""),
      `Grade 5+ Eng & Math (%)` = ifelse(`Grade 5+ Eng & Math (%)` %in% c("NE", "SUPP"), NA, `Grade 5+ Eng & Math (%)`),
      `Grade 5+ Eng & Math (%)` = as.numeric(`Grade 5+ Eng & Math (%)`)
    )
}


# STEP 3: Apply cleaned version
school_21_clean <- clean_school(school_21)
school_22_clean <- clean_school(school_22)
school_23_clean <- clean_school(school_23)

# STEP 4: Combine
school_tidy <- bind_rows(school_21_clean, school_22_clean, school_23_clean)

# STEP 5: Save
write_csv(school_tidy, "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/school_all_years_cleaned.csv")

