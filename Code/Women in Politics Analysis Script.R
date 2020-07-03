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


########################################################################
## Explore NAs ---------------------------------------------------------
########################################################################
missing_vals <- wp_cleaned %>%
  # gather(key = "key", value = "val") %>%
  pivot_longer(
    cols = wp_cleaned[, 1]:wp_cleaned[, length(wp_cleaned)],
    names_to = "variable",
    values_to = "value")


# The district column, at least for now, is all NAs, so let's remove it
