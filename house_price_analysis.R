library(tidyverse)
library(ggplot2)
library(scales)

house_prices <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/HousePrice-Folder/house-price_all_years_cleaned.csv")
house_prices %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"),
         Year %in% 2021:2024) %>%
  group_by(Year, County) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = as.integer(Year), y = AvgPrice, color = County)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  labs(title = "Average House Prices (2021–2024)",
       x = "Year", y = "Avg Price (£)") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

house_prices %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"), Year == 2023) %>%
  group_by(County) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = County, y = AvgPrice, fill = County)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Average House Prices by County (2023)",
       x = "County", y = "Avg Price (£)") +
  scale_y_continuous(labels = comma) +
  theme_minimal()


# South Yorkshire boxplot
house_prices %>%
  filter(County == "SOUTH YORKSHIRE", Year %in% 2021:2024) %>%
  ggplot(aes(x = factor(Year), y = Price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "South Yorkshire: House Prices (2021–2024)",
       x = "Year", y = "Price (£)") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# West Yorkshire boxplot
house_prices %>%
  filter(County == "WEST YORKSHIRE", Year %in% 2021:2024) %>%
  ggplot(aes(x = factor(Year), y = Price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "West Yorkshire: House Prices (2021–2024)",
       x = "Year", y = "Price (£)") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

