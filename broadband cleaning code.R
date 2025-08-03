# Load libraries
library(tidyverse)
library(readr)
library(stringr)

# Load datasets
broadband_perf <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Broadband-Folder/performance_clean.csv")
broadband_cov  <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Broadband-Folder/coverage_clean.csv")
postcode_lsoa <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Broadband-Folder/cleaned_postcode_lsoa.csv")

# Standardize postcode format to match others (insert space before last 3 characters)
broadband_perf <- broadband_perf %>%
  rename(postcode = `Postcode`, median_speed = `Median download speed (Mbit/s)`) %>%
  filter(!is.na(median_speed)) %>%
  mutate(postcode = str_replace_all(str_to_upper(postcode), "(\\w{3})$", " \\1"))

broadband_cov <- broadband_cov %>%
  rename(postcode = `Postcode`,
         SFBB = `SFBB availability (% premises)`,
         UFBB = `UFBB availability (% premises)`,
         FTTP = `FTTP availability (% premises)`) %>%
  mutate(postcode = str_replace_all(str_to_upper(postcode), "(\\w{3})$", " \\1"))


# Merge performance and coverage datasets
broadband_combined <- inner_join(broadband_perf, broadband_cov, by = "postcode")



# Merge with postcode-lsoa-district mapping
broadband_full <- inner_join(broadband_combined, postcode_lsoa, by = "postcode")


# Define target districts
sy_wy_districts <- c("Barnsley", "Doncaster", "Rotherham", "Sheffield",   # South Yorkshire
                     "Bradford", "Calderdale", "Kirklees", "Leeds", "Wakefield")  # West Yorkshire

# Filter postcode list to SY and WY
postcode_filtered <- postcode_lsoa %>%
  filter(district %in% sy_wy_districts)

# Merge broadband performance and coverage first
broadband_combined <- inner_join(broadband_perf, broadband_cov, by = "postcode")

#  Left join to keep all postcodes from SY/WY, even if missing broadband data
broadband_full <- postcode_filtered %>%
  left_join(broadband_combined, by = "postcode")

# Save final cleaned versions
write_csv(perf_final, "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Broadband-Folder/performance-FinalYorkshire.csv")
write_csv(cov_final, "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Broadband-Folder/coverage-FinalYorkshire.csv")
write_csv(broadband_full, "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Broadband-Folder/broadband_full.csv")
