# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load data
house_prices <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/HousePrice-Folder/avg-house-price(by-year).csv")
broadband <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Broadband-Folder/broadband_full.csv")
crime <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_by_district.csv")
schools <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/cleaned_school.csv")

#Model1----------------------------------------------
# Prep
house_2023 <- filter(house_prices, Year == 2023)
broadband$district <- tolower(trimws(broadband$district))
broadband_avg <- broadband %>%
  group_by(district) %>%
  summarise(avg_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE))

df1 <- house_2023 %>%
  mutate(District = tolower(Town)) %>%
  left_join(broadband_avg, by = c("District" = "district")) %>%
  filter(!is.na(avg_price) & !is.na(avg_speed))

# Model
model1 <- lm(avg_price ~ avg_speed, data = df1)
summary(model1)
cor.test(df1$avg_price, df1$avg_speed)

# Plot
ggplot(df1, aes(x = avg_speed, y = avg_price)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "House Price vs Download Speed", x = "Average Speed (Mbps)", y = "Average House Price")


#Model2----------------------------------------------------------------------------
crime$District <- tolower(trimws(crime$District))
df2 <- df1 %>%
  left_join(crime, by = c("District" = "District")) %>%
  filter(!is.na(crime_rate))

model2 <- lm(avg_price ~ crime_rate, data = df2)
summary(model2)
cor.test(df2$avg_price, df2$crime_rate)

ggplot(df2, aes(x = crime_rate, y = avg_price)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "House Price vs Drug Offense Rate", x = "Drug Offense Rate", y = "Average House Price")

#Model3---------------------------------------------------------------------------------
# Collapse constituencies to general districts
school_avg <- schools %>%
  mutate(district_match = case_when(
    grepl("leeds", constituency, ignore.case = TRUE) ~ "leeds",
    grepl("bradford", constituency, ignore.case = TRUE) ~ "bradford",
    grepl("wakefield", constituency, ignore.case = TRUE) ~ "wakefield",
    grepl("sheffield", constituency, ignore.case = TRUE) ~ "sheffield",
    grepl("doncaster", constituency, ignore.case = TRUE) ~ "doncaster",
    grepl("barnsley", constituency, ignore.case = TRUE) ~ "barnsley",
    grepl("rotherham", constituency, ignore.case = TRUE) ~ "rotherham",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(district_match)) %>%
  group_by(district_match) %>%
  summarise(attain8 = mean(score, na.rm = TRUE))

# Join with house price dataset
df3 <- df1 %>%
  left_join(school_avg, by = c("District" = "district_match")) %>%
  filter(!is.na(attain8), !is.na(avg_price))

# Build linear model
model3 <- lm(avg_price ~ attain8, data = df3)
summary(model3)

# Correlation
cor.test(df3$avg_price, df3$attain8)

# Plot
ggplot(df3, aes(x = attain8, y = avg_price)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "House Price vs Attainment 8 Score",
       x = "Attainment 8 Score",
       y = "Average House Price (£)") +
  theme_minimal()


#Model4-------------------------------------------------------------------------------------
# Load cleaned crime rate data (per district)
crime <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_by_district.csv") %>%
  mutate(District = tolower(trimws(District)))

# Join Attainment 8 scores with crime rate
df4 <- school_avg %>%
  rename(District = district_match) %>%
  left_join(crime, by = "District") %>%
  filter(!is.na(attain8), !is.na(crime_rate))

# Build linear model
model4 <- lm(attain8 ~ crime_rate, data = df4)
summary(model4)

# Correlation
cor.test(df4$attain8, df4$crime_rate)

# Plot
ggplot(df4, aes(x = crime_rate, y = attain8)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Attainment 8 Score vs Drug Offense Rate",
       x = "Drug Offense Rate (per 10,000)",
       y = "Attainment 8 Score") +
  theme_minimal()

#Model5---------------------------------------------------------------------------------
df5 <- df1 %>%
  left_join(crime, by = "District") %>%
  filter(!is.na(avg_speed), !is.na(crime_rate))

# Build linear model
model5 <- lm(avg_speed ~ crime_rate, data = df5)

summary(model5)

# Correlation
cor.test(df5$avg_speed, df5$crime_rate)

# Plot
ggplot(df5, aes(x = crime_rate, y = avg_speed)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Download Speed vs Drug Offense Rate",
       x = "Drug Offense Rate (per 10,000)",
       y = "Average Download Speed (Mbps)") +
  theme_minimal()


#Model6---------------------------------------------------------------------------------
df6 <- df1 %>%
  left_join(school_avg, by = c("District" = "district_match")) %>%
  filter(!is.na(attain8), !is.na(avg_speed))

# Build linear model
model6 <- lm(avg_speed ~ attain8, data = df6)
summary(model6)

# Correlation
cor.test(df6$avg_speed, df6$attain8)

# Plot
ggplot(df6, aes(x = attain8, y = avg_speed)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Download Speed vs Attainment 8 Score",
       x = "Attainment 8 Score",
       y = "Average Download Speed (Mbps)") +
  theme_minimal()

