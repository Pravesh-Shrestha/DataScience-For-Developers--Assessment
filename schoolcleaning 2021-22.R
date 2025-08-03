library(tidyverse)
library(readr)

# Step 1: Load the dataset
ks4_2021 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Obtained-Data/school dataset/2021-2022/england_ks4final.csv")

# Step 2: Define Local Authorities in South & West Yorkshire
yorkshire_leas <- c(370, 371, 373, 380, 383, 381, 382, 384, 385)

# Step 3: Filter and Clean
clean_school_21_22 <- ks4_2021 %>%
  filter(LEA %in% yorkshire_leas) %>%
  select(LEA, SCHNAME, ATT8SCR, P8MEA, PTL2BASICS_94) %>%
  rename(
    `Local Authority` = LEA,
    `School Name` = SCHNAME,
    `Attainment 8 Score` = ATT8SCR,
    `Progress 8 Score` = P8MEA,
    `Grade 5+ Eng & Math (%)` = PTL2BASICS_94
  ) %>%
  mutate(Year = "2021-22")

# Step 4: Save to Cleaned Folder
write_csv(clean_school_21_22, 
          "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/school_2021-22_cleaned.csv")
