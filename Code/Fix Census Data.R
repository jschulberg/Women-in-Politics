### Load in libraries
pacman::p_load(readxl, tidyverse)


### Read in data
census_data <- readxl::read_xlsx("Data/Census Decades.xlsx", sheet = 1)

# Bring in Alaska and Hawaii, which have a population of 0 during these years
# missing_states <- tibble(
#   Year = c("AK", "HI"),
#   `1900` = 0)

# census_data <- dplyr::bind_rows(census_data, missing_states)

for (i in 2:length(readxl::excel_sheets("Data/Census Decades.xlsx"))) {
  new_data <- readxl::read_xlsx("Data/Census Decades.xlsx", sheet = i)
  census_data <- merge(census_data, new_data, by = "Year")
  # census_data <- bind_cols(census_data, new_data)
}

# Our data now has 50 rows (one for each state) and a column for each year
# Let's pivot it to make it better formatted
census_pivoted <- census_data %>%
  tidyr::pivot_longer(cols = -"Year",
               names_to = "year",
               values_to = "population")

# Figure out our full state names
state_names <- tibble(state.abb, state.name)

# 1970 and later doesn't represent population in thousands, so let's standardize
# this
census_cleaned <- census_pivoted %>%
  mutate(population = if_else(year < 1970, population * 1000, population)) %>%
  # Change the name of our first column
  rename(state_code = Year) %>%
  merge(state_names, by.x = "state_code", by.y = "state.abb") %>%
  rename(state = state.name) %>%
  select(contains("state"), everything()) %>%
  arrange(state, year)

write_csv(census_cleaned, "Data/Census_Structured.csv")
