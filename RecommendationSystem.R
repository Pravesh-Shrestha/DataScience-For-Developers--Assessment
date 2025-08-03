# -------------------------------------------------------------------
# INVESTMENT RECOMMENDATION SYSTEM - Pravesh Kumar Shrestha
# -------------------------------------------------------------------
# GOAL: Recommend top 3 districts in South/West Yorkshire for property
# investment based on affordability, broadband speed, safety, and schools
# -------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(readr)
library(fmsb)
library(tidyr)

# STEP 1: LOAD DATA -------------------------------------------------
house <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/HousePrice-Folder/avg-house-price(by-year).csv")
broadband <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Broadband-Folder/broadband_full.csv")
crime <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/Crime-Folder/crime_summary_by_district.csv")
schools <- read_csv("C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Cleaned-Data/School-Folder/cleaned_school.csv")

# STEP 2: CLEAN AND STANDARDIZE -------------------------------------
house <- house %>% mutate(District = str_trim(toupper(Town)))
broadband <- broadband %>% rename(District = district) %>% mutate(District = str_trim(toupper(District)))
crime <- crime %>% mutate(District = str_trim(toupper(District)))

# STEP 3: SELECT VALID DISTRICTS ------------------------------------
valid_districts <- c("LEEDS", "BRADFORD", "WAKEFIELD", "KIRKLEES", "CALDERDALE",
                     "SHEFFIELD", "ROTHERHAM", "DONCASTER") # Barnsley excluded

# STEP 4: AGGREGATE CRIME (DRUGS) -----------------------------------
crime_drugs <- crime %>%
  filter(District %in% valid_districts) %>%
  group_by(District) %>%
  summarise(TotalDrugs = sum(crime_rate), .groups = "drop")

# STEP 5: AGGREGATE SCHOOL SCORES -----------------------------------
school_latest <- schools %>%
  filter(year == "2023-2024") %>%
  mutate(District = case_when(
    grepl("leeds", constituency, ignore.case = TRUE) ~ "LEEDS",
    grepl("bradford", constituency, ignore.case = TRUE) ~ "BRADFORD",
    grepl("wakefield", constituency, ignore.case = TRUE) ~ "WAKEFIELD",
    grepl("sheffield", constituency, ignore.case = TRUE) ~ "SHEFFIELD",
    grepl("doncaster", constituency, ignore.case = TRUE) ~ "DONCASTER",
    grepl("rotherham", constituency, ignore.case = TRUE) ~ "ROTHERHAM",
    grepl("kirklees", constituency, ignore.case = TRUE) ~ "KIRKLEES",
    grepl("calderdale", constituency, ignore.case = TRUE) ~ "CALDERDALE",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(District)) %>%
  group_by(District) %>%
  summarise(`Attainment 8 Score` = mean(score, na.rm = TRUE), .groups = "drop")

# STEP 6: COMBINE DATA ----------------------------------------------
recommend_data <- house %>%
  filter(Year == 2023, District %in% valid_districts) %>%
  left_join(broadband, by = "District") %>%
  left_join(crime_drugs, by = "District") %>%
  left_join(school_latest, by = "District") %>%
  filter(!is.na(avg_price),
         !is.na(`Average download speed (Mbit/s)`),
         !is.na(TotalDrugs),
         !is.na(`Attainment 8 Score`)) %>%
  distinct(District, .keep_all = TRUE)

# STEP 7: SCORING FUNCTION ------------------------------------------
normalize <- function(x, reverse = FALSE) {
  if (all(is.na(x)) || length(unique(x[!is.na(x)])) == 1) {
    return(rep(0, length(x)))
  }
  rng <- range(x, na.rm = TRUE)
  if (reverse) {
    return(10 * (rng[2] - x) / (rng[2] - rng[1]))
  } else {
    return(10 * (x - rng[1]) / (rng[2] - rng[1]))
  }
}

# STEP 8: SCORE AND RANK --------------------------------------------
recommend_data <- recommend_data %>%
  mutate(
    Score_House  = normalize(avg_price, reverse = TRUE),
    Score_Speed  = normalize(`Average download speed (Mbit/s)`),
    Score_Crime  = normalize(TotalDrugs, reverse = TRUE),
    Score_School = normalize(`Attainment 8 Score`)
  ) %>%
  filter(!if_any(starts_with("Score_"), is.na)) %>%
  rowwise() %>%
  mutate(Total_Score = mean(c_across(starts_with("Score_")), na.rm = TRUE)) %>%
  ungroup()

# STEP 9: TOP 3 RECOMMENDATIONS -------------------------------------
top_recommendations <- recommend_data %>%
  arrange(desc(Total_Score)) %>%
  select(District, Total_Score, Score_House, Score_Speed, Score_Crime, Score_School) %>%
  head(3)

print(top_recommendations)

# STEP 10: VISUALIZATION --------------------------------------------
top_long <- top_recommendations %>%
  pivot_longer(cols = starts_with("Score_"), names_to = "Criterion", values_to = "Score") %>%
  mutate(Criterion = recode(Criterion,
                            Score_House = "House Price",
                            Score_Speed = "Broadband Speed",
                            Score_Crime = "Safety (Low Crime)",
                            Score_School = "School Performance")) %>%
  group_by(District) %>%
  arrange(District, desc(Criterion)) %>%
  mutate(pos = cumsum(Score) - 0.5 * Score)

ggplot(top_long, aes(x = reorder(District, -Total_Score), y = Score, fill = Criterion)) +
  geom_col(width = 0.6, color = "white", linewidth = 0.3) +
  geom_text(aes(y = pos, label = round(Score, 1)), color = "black", size = 4) +
  labs(
    title = "Top 3 Recommended Towns – Score Breakdown",
    subtitle = "Each bar shows the combined score based on 4 key factors (0–10 scale)",
    x = "District",
    y = "Normalized Score (0–10)",
    fill = "Scoring Factor"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top"
  )
# STEP 10: EXPORT TOP 3 DATA ----------------------------------------
write_csv(top_recommendations, "C:/Users/sthap/Documents/DataScience-Assignment/Coursework-Pravesh Kumar Shrestha-230423/Recommendation-System/top3_recommendations.csv")
