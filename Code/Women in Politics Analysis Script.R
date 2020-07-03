###########################################################################
###########################################################################

#######                  Women in Politics Analysis                 #######

###########################################################################
###########################################################################
# In this script, I will analyze the Women in Politics dataset included in
# the data folder of this repository. I will use a variety of exploratory
# and modeling techniques to answer the following questions:

# How many


###########################################################################
## Set Up -----------------------------------------------------------------
###########################################################################
# Bring in packages
suppressMessages(library("tidyverse")) # Used for data wrangling
suppressMessages(library("tidyr")) # Used for data cleaning
suppressMessages(library("ggplot2")) # Used for visualizations
suppressMessages(library("readxl")) # Used for loading excel files
suppressMessages(library("readr")) # Used for working with files
suppressMessages(library("pander")) # Used for pretty tables
suppressMessages(library("lubridate")) # Used for fixing dates
suppressMessages(library("praise")) # Used for positive reinforcement
suppressMessages(library("janitor")) # Used for data cleaning


# Bring in the data, taking advantage of the project structure
women_politics_data <- readr::read_csv(here::here("Data/women_in_politics.csv"))

# Convert to a tibble, my preferred data structure
(women_politics_data <- as_tibble(women_politics_data))


########################################################################
## Clean Data ----------------------------------------------------------
########################################################################
# Some of the data is a bit messy, so let's clean things up
wp_cleaned <- women_politics_data %>%
  # Clean up our column names so they're in a more standard format
  clean_names() %>%
  # Create and clean up a few columns
  # The state variable is of the form "Michigan - MI", so let's break this
  # into something more readable
  separate(state,
           into = c("state", "state_abb"),
           sep = "-") %>%
  mutate(
    # The state columns now have a big of extra white space
    state = str_trim(state),
    state_abb = str_trim(state_abb),
    # Because the short code for Northern Mariana Islands (MI) matches
    # that of Michigan, let's change the code
    state_abb = if_else(state == "Northern Mariana Islands", "NMI", state_abb),
    # Create a full name field
    full_name = if_else(is.na(middle_name),
                        paste(first_name, last_name),
                        paste(first_name, middle_name, last_name))
  ) %>%
  print()

# Our dataset is structured such that each row corresponds to one year that
# a woman held elected office. Let's create two new variables:
#   min_year: the first year the woman held office
#   max_year: the last year the woman held office
wp_minmax <- wp_cleaned %>%
  group_by(id) %>%
  mutate(min_year = min(year),
         max_year = max(year),
         years_of_service = paste(min(year), max(year), sep = "-")) %>%
  # filter(year == min(year) |
  #          year == max(year)) %>%
  select(id, contains("year"), contains("name"), everything()) %>%
  print()


########################################################################
## Explore NAs ---------------------------------------------------------
########################################################################
# Start by creating a dataframe that will store our missing values
missing_vals <- wp_minmax %>%
  # Pivot the dataset so all of our variables are in two columns
  gather(key = "key", value = "val") %>%
  # Figure out which values are missing
  mutate(is_missing = is.na(val)) %>%
  # Group by our variable and whether or not a value is missing
  group_by(key, is_missing) %>%
  # Count the number of values missing for each variable
  summarise(num_missing = n()) %>%
  # Only pull in the variables that are missing
  filter(is_missing == T) %>%
  # Get rid of our logical field
  select(-is_missing) %>%
  # Sort our values descending
  arrange(desc(num_missing)) %>%
  # Ungroup our results
  ungroup()

# Let's take a look at our results
pander(missing_vals)
# The district column, at least for now, is all NAs, so let's remove it.
# Middle name, which is about 1/3 null, is fine since that's not an essential
# variable

wp_selected <- wp_minmax %>%
  select(-district)
