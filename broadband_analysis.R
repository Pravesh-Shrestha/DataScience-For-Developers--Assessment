# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load broadband dataset
cleaned_broadband <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Broadband-Folder/broadband_full.csv")

# Step 1: Assign counties manually from district
cleaned_broadband <- cleaned_broadband %>%
  mutate(
    county = case_when(
      district %in% c("Barnsley", "Doncaster", "Rotherham", "Sheffield") ~ "South Yorkshire",
      district %in% c("Leeds", "Wakefield", "Bradford", "Kirklees", "Calderdale") ~ "West Yorkshire",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(county)) %>%
  rename(town = shortPostcode.x) %>%
  filter(!is.na(town), town != "")

# ============================
# 📦 BOXPLOTS: District vs Speed
# ============================

# Boxplot – South Yorkshire
ggplot(cleaned_broadband %>% filter(county == "South Yorkshire"), 
       aes(x = district, y = `Average download speed (Mbit/s)`)) +
  geom_boxplot(fill = "red") +
  labs(title = "Download Speed by District – South Yorkshire",
       x = "District", y = "Average Download Speed (Mbps)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot – West Yorkshire
ggplot(cleaned_broadband %>% filter(county == "West Yorkshire"), 
       aes(x = district, y = `Average download speed (Mbit/s)`)) +
  geom_boxplot(fill = "purple") +
  labs(title = "Download Speed by District – West Yorkshire",
       x = "District", y = "Average Download Speed (Mbps)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================
# 📊 BARCHARTS: Top 10 Towns by Speed
# ============================

# Calculate average speed by town and county
town_speed <- cleaned_broadband %>%
  group_by(county, town) %>%
  summarise(avg_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE), .groups = "drop")

# Bar chart – Top 10 towns South Yorkshire
ggplot(town_speed %>% filter(county == "South Yorkshire") %>% top_n(10, avg_speed), 
       aes(x = reorder(town, avg_speed), y = avg_speed)) +
  geom_col(fill = "navyblue") +
  coord_flip() +
  labs(title = "Top 10 Towns by Download Speed – South Yorkshire",
       x = "Town", y = "Avg Download Speed (Mbps)") +
  theme_minimal()

# Bar chart – Top 10 towns West Yorkshire
ggplot(town_speed %>% filter(county == "West Yorkshire") %>% top_n(10, avg_speed), 
       aes(x = reorder(town, avg_speed), y = avg_speed)) +
  geom_col(fill = "lightgreen") +
  coord_flip() +
  labs(title = "Top 10 Towns by Download Speed – West Yorkshire",
       x = "Town", y = "Avg Download Speed (Mbps)") +
  theme_minimal()
