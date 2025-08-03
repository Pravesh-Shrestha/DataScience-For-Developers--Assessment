# Load libraries
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# Define base path for raw CSVs
base_path <- "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Obtained-Data/Houses Prices/"
files <- c("pp-2021.csv", "pp-2022.csv", "pp-2023.csv", "pp-2024.csv")

# Assign proper column names
column_names <- c(
  "TransactionID", "Price", "Date", "Postcode", "PropertyType", "NewBuild", "Tenure",
  "PAON", "SAON", "Street", "Locality", "Town", "District", "County",
  "PPD_Category", "Record_Status"
)

# Define target towns in South and West Yorkshire
target_towns <- c("sheffield", "barnsley", "doncaster", "rotherham",   # South Yorkshire
                  "leeds", "bradford", "huddersfield", "wakefield")    # West Yorkshire

# Read, combine, clean
house_prices <- lapply(files, function(file) {
  read_csv(paste0(base_path, file), col_names = FALSE) %>%
    `colnames<-`(column_names)
}) %>% bind_rows()

# Clean data
house_prices_clean <- house_prices %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    Town = str_trim(tolower(Town)),      # standardize town names
    District = str_trim(tolower(District)),
    County = str_trim(tolower(County)),
    Price = as.numeric(Price)
  ) %>%
  filter(Town %in% target_towns)         # only include West and South Yorkshire towns

# Optional: remove price outliers
Q1 <- quantile(house_prices_clean$Price, 0.25, na.rm = TRUE)
Q3 <- quantile(house_prices_clean$Price, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

house_prices_clean <- house_prices_clean %>%
  filter(Price > (Q1 - 1.5 * IQR), Price < (Q3 + 1.5 * IQR))

# Save cleaned dataset
write_csv(house_prices_clean,
          "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/HousePrice-Folder/house-price_all_years_cleaned.csv")

# Summarize yearly average prices per town
avg_price_by_year <- house_prices_clean %>%
  group_by(Town, Year) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")

# Save summary
write_csv(avg_price_by_year,
          "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/HousePrice-Folder/avg-house-price(by-year).csv")

# Optional view
print(head(avg_price_by_year))
