# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load data
house_prices <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423-FOLDER/Cleaned-Data/HousePrice-Folder/avg-house-price(by-year).csv")
broadband <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423-FOLDER/Cleaned-Data/Broadband-Folder/broadband_full.csv")
crime <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423-FOLDER/Cleaned-Data/Crime-Folder/crime_summary_by_district.csv")
schools <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423-FOLDER/Cleaned-Data/School-Folder/cleaned_school.csv")


# Normalize names
house_prices <- house_prices %>% mutate(District = tolower(Town))
broadband <- broadband %>% mutate(district = tolower(trimws(district)))
crime <- crime %>% mutate(District = tolower(trimws(District)))

# Add County tag
house_prices <- house_prices %>% mutate(County = ifelse(District %in% c("leeds", "wakefield", "bradford", "calderdale", "kirklees"), "West Yorkshire", "South Yorkshire"))
broadband <- broadband %>% mutate(County = ifelse(district %in% c("leeds", "wakefield", "bradford", "calderdale", "kirklees"), "West Yorkshire", "South Yorkshire"))
crime <- crime %>% mutate(County = ifelse(District %in% c("leeds", "wakefield", "bradford", "calderdale", "kirklees"), "West Yorkshire", "South Yorkshire"))

# PREP -------------------------------------------------------------
# House prices 2023
house_2023 <- house_prices %>% filter(Year == 2023)
broadband$district <- tolower(trimws(broadband$district))
broadband_avg <- broadband %>%
  group_by(district) %>%
  summarise(avg_speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE))

# Merge house & broadband
df_base <- house_2023 %>%
  mutate(District = tolower(Town)) %>%
  left_join(broadband_avg, by = c("District" = "district")) %>%
  filter(!is.na(avg_price) & !is.na(avg_speed))

# Crime
crime <- crime %>%
  mutate(District = tolower(trimws(District)))

# School scores
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
  summarise(attain8 = mean(score, na.rm = TRUE)) %>%
  rename(District = district_match)

# Add county info
df_all <- df_base %>%
  left_join(crime, by = "District") %>%
  left_join(school_avg, by = "District") %>%
  mutate(County = case_when(
    District %in% c("barnsley", "doncaster", "rotherham", "sheffield") ~ "South Yorkshire",
    District %in% c("leeds", "bradford", "wakefield", "kirklees", "calderdale") ~ "West Yorkshire",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(County))

# BASE PLOT FUNCTION -----------------------------------------------
plot_model <- function(df, xvar, yvar, xtitle, ytitle, plot_title, filename) {
  ggplot(df, aes_string(x = xvar, y = yvar, color = "County")) +
    geom_point(size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, size = 1.2) +
    labs(
      title = plot_title,
      subtitle = "Linear Regression Comparison Between South and West Yorkshire (2023)",
      x = xtitle,
      y = ytitle,
      color = "County"
    ) +
    scale_color_manual(values = c("South Yorkshire" = "#FF5733", "West Yorkshire" = "#0072B2")) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
      axis.title.x = element_text(size = 13, margin = margin(t = 10)),
      axis.title.y = element_text(size = 13, margin = margin(r = 10)),
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
}

# GENERATE PLOTS ----------------------------------------------------

# Model 1: House Price vs Download Speed
plot_model(df_all, "avg_speed", "avg_price", "Average Download Speed (Mbps)", "Average House Price (£)",
           "Model 1: House Price vs Download Speed", "model1_price_vs_speed.png")

# Model 2: House Price vs Drug Offense Rate
plot_model(df_all, "crime_rate", "avg_price", "Drug Offense Rate (per 10,000)", "Average House Price (£)",
           "Model 2: House Price vs Drug Offense Rate", "model2_price_vs_crime.png")

# Model 3: House Price vs Attainment 8 Score
plot_model(df_all, "attain8", "avg_price", "Attainment 8 Score", "Average House Price (£)",
           "Model 3: House Price vs Attainment 8 Score", "model3_price_vs_school.png")

# Model 4: School Score vs Drug Offense Rate
plot_model(df_all, "crime_rate", "attain8", "Drug Offense Rate (per 10,000)", "Attainment 8 Score",
           "Model 4: Attainment 8 Score vs Drug Offense Rate", "model4_school_vs_crime.png")

# Model 5: Download Speed vs Drug Offense Rate
plot_model(df_all, "crime_rate", "avg_speed", "Drug Offense Rate (per 10,000)", "Average Download Speed (Mbps)",
           "Model 5: Download Speed vs Drug Offense Rate", "model5_speed_vs_crime.png")

# Model 6: Download Speed vs Attainment 8 Score
plot_model(df_all, "attain8", "avg_speed", "Attainment 8 Score", "Average Download Speed (Mbps)",
           "Model 6: Download Speed vs Attainment 8 Score", "model6_speed_vs_school.png")


# LINEAR MODELLING ANALYSIS WITH STATISTICS AND PLOTS
library(ggplot2)

plot_and_model <- function(data, xvar, yvar, xtitle, ytitle, title) {
  # Model
  formula <- as.formula(paste(yvar, "~", xvar))
  model <- lm(formula, data = data)
  print(paste("\n---", title, "---"))
  print(summary(model))
  print(cor.test(data[[xvar]], data[[yvar]]))
  
  # Plot
  p <- ggplot(data, aes_string(x = xvar, y = yvar, color = "County")) +
    geom_point(size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, size = 1.2) +
    labs(
      title = title,
      subtitle = "Linear Regression Comparison Between South and West Yorkshire (2023)",
      x = xtitle,
      y = ytitle,
      color = "County"
    ) +
    scale_color_manual(values = c("South Yorkshire" = "#FF5733", "West Yorkshire" = "#0072B2")) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
      axis.title.x = element_text(size = 13, margin = margin(t = 10)),
      axis.title.y = element_text(size = 13, margin = margin(r = 10)),
      legend.position = "bottom",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11)
    )
  print(p)
}

# Model 1: House Price vs Download Speed
plot_and_model(df_all, "avg_speed", "avg_price", "Average Download Speed (Mbps)", "Average House Price (\u00a3)", "Model 1: House Price vs Download Speed")

# Model 2: House Price vs Drug Offense Rate
plot_and_model(df_all, "crime_rate", "avg_price", "Drug Offense Rate (per 10,000)", "Average House Price (\u00a3)", "Model 2: House Price vs Drug Offense Rate")

# Model 3: Attainment 8 Score vs House Price
plot_and_model(df_all, "attain8", "avg_price", "Attainment 8 Score", "Average House Price (\u00a3)", "Model 3: Attainment 8 Score vs House Price")

# Model 4: Attainment 8 Score vs Drug Offense Rate
plot_and_model(df_all, "crime_rate", "attain8", "Drug Offense Rate (per 10,000)", "Attainment 8 Score", "Model 4: Attainment 8 Score vs Drug Offense Rate")

# Model 5: Download Speed vs Drug Offense Rate
plot_and_model(df_all, "crime_rate", "avg_speed", "Drug Offense Rate (per 10,000)", "Average Download Speed (Mbps)", "Model 5: Download Speed vs Drug Offense Rate")

# Model 6: Download Speed vs Attainment 8 Score
plot_and_model(df_all, "attain8", "avg_speed", "Attainment 8 Score", "Average Download Speed (Mbps)", "Model 6: Download Speed vs Attainment 8 Score")


