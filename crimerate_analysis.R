# ====== Libraries ======
library(tidyverse)
library(fmsb)      # Radar chart
library(scales)    # Axis formatting
library(lubridate) # Month extraction

# ====== Load Data ======
crime_district <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_by_district.csv")
crime_data <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_total.csv")

crime_2022 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_2022.csv") %>% mutate(Year = 2022)
crime_2023 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_2023.csv") %>% mutate(Year = 2023)
crime_2024 <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_2024.csv") %>% mutate(Year = 2024)

crime_all <- bind_rows(crime_2022, crime_2023, crime_2024)

# ====== Add County and Month Name ======
crime_all <- crime_all %>%
  mutate(
    County = case_when(
      District %in% c("Barnsley", "Doncaster", "Rotherham", "Sheffield") ~ "South Yorkshire",
      District %in% c("Leeds", "Wakefield", "Bradford", "Kirklees", "Calderdale") ~ "West Yorkshire",
      TRUE ~ NA_character_
    ),
    Month_Name = month.name[month(ymd(Month))]
  ) %>%
  filter(!is.na(County))

# ====== 1. Boxplots: Drug Offense Rate by District ======

# Create a summarized table by district for drug-related offenses
drug_district <- crime_all %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(County, District) %>%
  summarise(crime_rate = mean(Crime_Count), .groups = "drop")

# South Yorkshire
drug_district %>%
  filter(County == "South Yorkshire") %>%
  ggplot(aes(x = District, y = crime_rate)) +
  geom_boxplot(fill = "#FF6B6B") +
  labs(title = "Drug Offense Rate - South Yorkshire",
       x = "District", y = "Average Monthly Offense Count") +
  theme_minimal()

# West Yorkshire
drug_district %>%
  filter(County == "West Yorkshire") %>%
  ggplot(aes(x = District, y = crime_rate)) +
  geom_boxplot(fill = "#4ECDC4") +
  labs(title = "Drug Offense Rate - West Yorkshire",
       x = "District", y = "Average Monthly Offense Count") +
  theme_minimal()

# ====== Preprocess Date Column ======
crime_data <- crime_data %>%
  mutate(Month = ymd(Month))

# ====== Define South Yorkshire Districts ======
south_yorkshire_districts <- c("Barnsley", "Doncaster", "Rotherham", "Sheffield")

# ====== Filter Data for South Yorkshire, March 2024, Vehicle Crime ======
vehicle_sy_mar2024 <- crime_data %>%
  filter(District %in% south_yorkshire_districts,
         `Crime type` == "Vehicle crime",
         year(Month) == 2024,
         month(Month) == 3) %>%
  group_by(District) %>%
  summarise(CrimeRate = sum(Crime_Count), .groups = "drop")

# ====== Define max value and step for axis labels ======
max_val <- max(vehicle_sy_mar2024$CrimeRate) + 50
step <- 50

# ====== Create radar data frame ======
radar_df <- rbind(
  rep(max_val, nrow(vehicle_sy_mar2024)),  # Max values for scale
  rep(0, nrow(vehicle_sy_mar2024)),        # Min values for scale
  vehicle_sy_mar2024$CrimeRate             # Actual crime data
)

radar_df <- as.data.frame(radar_df)
colnames(radar_df) <- vehicle_sy_mar2024$District
rownames(radar_df) <- c("Max", "Min", "VehicleCrime")

# ====== Plot Radar Chart ======
radarchart(radar_df,
           axistype = 1,
           pcol = "#2196F3",
           pfcol = rgb(33, 150, 243, alpha = 100, maxColorValue = 255),
           plwd = 2,
           cglcol = "grey",
           cglty = 1,
           cglwd = 0.8,
           axislabcol = "black",
           caxislabels = seq(0, max_val, by = step),
           vlcex = 1.0,
           title = "Vehicle Crime Rate - South Yorkshire (March 2024)")


# ====== 3. Pie Chart: Robbery Rate (South Yorkshire - June 2023) ======
robbery_june23 <- crime_all %>%
  filter(County == "South Yorkshire",
         `Crime type` == "Robbery",
         Year == 2023,
         Month_Name == "June") %>%
  group_by(District) %>%
  summarise(CrimeRate = sum(Crime_Count), .groups = "drop") %>%
  arrange(desc(CrimeRate))

ggplot(robbery_june23, aes(x = "", y = CrimeRate, fill = District)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Robbery Distribution - South Yorkshire (June 2023)") +
  theme_void()

# ====== 4. Line Chart: Drug Offense Rate per 10,000 People (2022–2024) ======
# Dummy population (replace with actual data if available)
population_data <- tibble(
  County = rep(c("South Yorkshire", "West Yorkshire"), each = 3),
  Year = rep(c(2022, 2023, 2024), times = 2),
  Population = c(1410000, 1420000, 1430000, 2320000, 2330000, 2340000)
)

drug_rate <- crime_all %>%
  filter(`Crime type` == "Drugs") %>%
  group_by(County, Year) %>%
  summarise(TotalDrugs = sum(Crime_Count), .groups = "drop") %>%
  left_join(population_data, by = c("County", "Year")) %>%
  mutate(RatePer10k = round((TotalDrugs / Population) * 10000, 2))

ggplot(drug_rate, aes(x = Year, y = RatePer10k, color = County)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("South Yorkshire" = "#E76F51", "West Yorkshire" = "#264653")) +
  labs(title = "Drug Offense Rate per 10,000 People (2022–2024)",
       x = "Year", y = "Rate per 10,000 People") +
  theme_minimal() +
  scale_y_continuous(labels = comma)

