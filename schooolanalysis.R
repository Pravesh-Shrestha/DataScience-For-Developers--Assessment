# ===== Load and clean the dataset =====
school_data <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/school_all_years_cleaned.csv")

# Load packages
library(ggplot2)
library(dplyr)
library(readr)


# Standardize column names
colnames(school_data) <- tolower(gsub(" ", "_", colnames(school_data)))

# Convert year format like "2022-23" → 2022
school_data$year <- as.numeric(sub("-.*", "", unlist(school_data$year)))

# Filter for year 2022 only
school_2022 <- school_data %>% filter(year == 2022)

# Define districts
south_yorkshire <- c("Sheffield", "Doncaster", "Barnsley", "Rotherham")
west_yorkshire  <- c("Leeds", "Bradford", "Wakefield", "Kirklees", "Calderdale")

# === BOX PLOT – South Yorkshire ===
south_data_2022 <- school_2022 %>% filter(local_authority %in% south_yorkshire)

ggplot(south_data_2022, aes(x = local_authority, y = attainment_8_score)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Attainment 8 Score (2022) – South Yorkshire",
    x = "District",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal()

# === BOX PLOT – West Yorkshire ===
west_data_2022 <- school_2022 %>% filter(local_authority %in% west_yorkshire)

ggplot(west_data_2022, aes(x = local_authority, y = attainment_8_score)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Attainment 8 Score (2022) – West Yorkshire",
    x = "District",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal()

# ===============================
# 3. LINE GRAPH – Trend from 2021 to 2024
# ===============================
trend_districts <- c(south_yorkshire, west_yorkshire)

trend_data <- school_data %>%
  filter(local_authority %in% trend_districts)

ggplot(trend_data, aes(x = year, y = attainment_8_score, color = local_authority)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  labs(
    title = "Trend of Attainment 8 Score (2021–2024)",
    x = "Year",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal()

