library(tidyverse)

# Load population
population_clean <- read_csv("cleaned_population.csv")

# Clean and group
population_ready <- population_clean %>%
  mutate(
    shortPostcode = str_replace_all(shortPostcode, "\\s+", ""), # Remove all spaces
    Population = as.numeric(Population)
  ) %>%
  group_by(shortPostcode) %>%
  summarise(Population2023 = sum(Population, na.rm = TRUE), .groups = "drop")

# Save cleaned file
write_csv(population_ready, "population_grouped_clean.csv")
