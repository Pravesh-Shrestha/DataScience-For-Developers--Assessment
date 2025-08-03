library(tidyverse)
library(readr)

# Step 1: Load the dataset
ks4_2022 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Obtained-Data/school dataset/2022-2023/england_ks4final.csv")

# Step 2: Define Yorkshire LEAs (same as before)
yorkshire_leas <- c(370, 371, 373, 380, 383, 381, 382, 384, 385)

# Step 3: Clean and Filter
clean_school_22_23 <- ks4_2022 %>%
  filter(LEA %in% yorkshire_leas) %>%
  select(LEA, SCHNAME, ATT8SCR, P8MEA, PTL2BASICS_94) %>%
  rename(
    `Local Authority` = LEA,
    `School Name` = SCHNAME,
    `Attainment 8 Score` = ATT8SCR,
    `Progress 8 Score` = P8MEA,
    `Grade 5+ Eng & Math (%)` = PTL2BASICS_94
  ) %>%
  mutate(Year = "2022-23")

# Step 4: Save the Cleaned File
write_csv(clean_school_22_23,
          "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/school_2022-23_cleaned.csv")
