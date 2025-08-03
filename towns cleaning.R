HousePrices <- read_csv("house_prices_all_years_clean.csv")
PopulationData <- read_csv("population_cleaned.csv")

Towns <- HousePrices %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  mutate(shortPostcode = str_sub(Postcode, 1, 4)) %>%
  left_join(PopulationData, by = "shortPostcode") %>%
  select(shortPostcode, Town, District, County, 
         Population2020, Population2021, Population2022, Population2023, Population2024) %>%
  group_by(shortPostcode) %>%
  filter(row_number() == 1) %>%
  arrange(County)

write_csv(Towns, "Towns.csv")
