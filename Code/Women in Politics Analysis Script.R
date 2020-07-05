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
suppressMessages(library("pdftools")) # Used reading PDF files in


# Bring in the data, taking advantage of the project structure
# Our base dataset
female_politicians_data <- readr::read_csv(here::here("Data/women_in_politics.csv"))
# Read in a pdf of the number of House members over time, by state
house_members <- here::here("Data/state_apportionment.pdf") %>%
  pdftools::pdf_text() %>%
  readr::read_lines()

# Convert to a tibble, my preferred data structure
(female_politicians_data <- as_tibble(female_politicians_data))
(house_members <- as_tibble(house_members))


########################################################################
## Clean Data ----------------------------------------------------------
########################################################################
# Some of the data is a bit messy, so let's clean things up
wp_cleaned <- female_politicians_data %>%
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
                        str_c(first_name, last_name, sep = " "),
                        str_c(first_name, middle_name, last_name, sep = " ")),
    # There are a lot of small political parties (outside of Dem/Repub) that
    # appear in the dataset, so let's create a grouped column
    party_grouped = case_when(
      party == "Democrat" ~ "Democrat",
      party == "Republican" ~ "Republican",
      TRUE ~ "Other")
  ) %>%
  # Reorder columns to see the result
  select(id, contains("name"), contains("state"), everything()) %>%
  print()

# Our dataset is structured such that each row corresponds to one year that
# a woman held elected office. Let's create two new variables:
#   min_year: the first year the woman held office
#   max_year: the last year the woman held office
wp_minmax <- wp_cleaned %>%
  group_by(id) %>%
  mutate(min_year = min(year),
         max_year = max(year),
         years_of_service = str_c(min(year), max(year), sep = "-")) %>%
  # Reorder columns to see the result
  select(id, contains("year"), contains("name"), everything()) %>%
  ungroup() %>%
  print()


### House Dataset
# Let's clean up our dataset of the number of total members in the House
# by State over time
house_cleaned <- house_members %>%
  str_squish() %>%
  str_split(pattern = " ") %>%
  unlist() %>%
  as_tibble() %>%
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

# Now that we have our data, let's start to explore it using some fancy
# visualizations

########################################################################
## Viz Time ------------------------------------------------------------
########################################################################
# Before we jump into visualizations, let's define our political party
# colors
party_colors <- tibble(
  party_colors = c("#2E74C0", "#CB454A", "#999999"),
  party_grouped = c("Democrat", "Republican", "Other")
)

# Total number of Female Politicians holding office by Level
wp_selected %>%
  # Select our variables to analyze
  select(id, level, party_grouped) %>%
  # Pull only distinct values
  distinct() %>%
  # Group by level and political party
  group_by(level, party_grouped) %>%
  # Count everything up!
  summarise(num = n()) %>%
  # Bring our colors back in
  left_join(party_colors, by = c("party_grouped", "party_grouped")) %>%
  # Start our visualization, creating our groups by party affiliation
  ggplot(aes(x = reorder(party_grouped, num), y = num, fill = party_grouped)) +
  geom_col() +
  # Change our color scales
  scale_fill_manual(name = "Party", values = c("#2E74C0", "#999999", "#CB454A")) +
  # Create a separate chart, with a flexible y-axis, for each level of office
  facet_wrap(~level, scales = "free_y") +
  # Change the theme to classic
  theme_classic() +
  # Let's change the names of the axes and title
    xlab("Party") +
    ylab("Number of Female Politicians") +
    labs(title = "Number of Female Politicians at Various\nLevels of Government",
         subtitle = paste("Data ranges from", min(wp_selected$year), "to", max(wp_selected$year)),
         caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
    # format our title and subtitle
    theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
          plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
          plot.caption = element_text(color = "dark gray", size = 10, face = "italic"))


# Quite clearly, most of the women who have held office have done so at the state legislative
# level. Interestingly enough, most of the women who have held office at the state
# legislative or Federal/Congress level have been members of the two major parties.
# In D.C., a lot of women outside the two major parties have won office.


# Let's break out these numbers of Female Politicians holding office over time
wp_selected %>%
  # Select our variables to analyze
  select(id, level, party_grouped, year) %>%
  # Pull only distinct values
  distinct() %>%
  # Group by level and political party
  group_by(year, level, party_grouped) %>%
  # Count everything up!
  summarise(num = n()) %>%
  # Bring our colors back in
  left_join(party_colors, by = c("party_grouped", "party_grouped")) %>%
  # Because it'll skew the data, let's get rid of anything from the current
  # year or later
  filter(year < year(Sys.Date())) %>%
  # Start our visualization, creating our groups by party affiliation
  ggplot(aes(x = year, y = num, color = party_grouped)) +
  geom_line(lwd = 2) +
  # Change our color scales
  scale_color_manual(name = "Party", values = c("#2E74C0", "#999999", "#CB454A")) +
  # Create a separate chart, with a flexible y-axis, for each level of office
  facet_wrap(~level, scales = "free_y") +
  # Change the theme to classic
  theme_classic() +
  # Let's change the names of the axes and title
  xlab("Year") +
  ylab("Number of Female Politicians") +
  labs(title = "Number of Female Politicians at Various\nLevels of Government over Time",
       subtitle = paste("Data ranges from", min(wp_selected$year), "to", max(wp_selected$year)),
       caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic"))

# Here are some key takeaways:
#   1. The number of women holding elected office has been growing incredibly
#      fast over the past few decades
#   2. For female politicians in Congress, Democrats and Repulicans held
#      lock-step until 1990, when Democrats took off at a much faster rate
#   3. Most of the positions held are at the State Legislative level, which
#      makes sense given the higher number of positions open at that level.


# Now let's explore the proportion of women who have been members of the U.S.
# Senate over time. Unfortunately our dataset only includes the number of
# women who have held elected office over time, so it's difficult to compare
# that to the number of men who have held office over time at all the levels
# of government, except for the U.S. Senate, which had 100 members since 1959.
senate_composition <- wp_selected %>%
  # Filter to only our U.S. Senators after 1958
  filter(level == "Federal/Congress" &
           position == "U.S. Senator" &
           year > 1958 &
           year < year(Sys.Date())) %>%
  # Group by year
  group_by(year) %>%
  # Count everything up!
  summarise(female_senators = n(),
            male_senators = 100 - n()) %>%
  # Pivot our dataset longer so we have number of senators in one column
  # instead of two
  pivot_longer(cols = contains("senators"),
               names_to = "gender",
               values_to = "politicians") %>%
  # Clean up the gender column since it has "_senators" at the end of it
  mutate(gender = str_remove(gender, pattern = "_.*"),
         # Make the first letter upper case
         gender = str_to_title(gender),
         # Make it a factor
         gender = factor(gender, levels = c("Male", "Female")),
         # Let's create one column to identify this dataset as Senate
         branch = "U.S. Senate") %>%
  print()

# Let's do the same thing, but for U.S. House of Representatives, which has
# 435 members every year
house_composition <- wp_selected %>%
  # Filter to only our U.S. House of Representatives members after 1958
  filter(level == "Federal/Congress" &
           position == "U.S. Representative" &
           year > 1958 &
           year < year(Sys.Date())) %>%
  # Group by year
  group_by(year) %>%
  # Count everything up!
  summarise(female_reps = n(),
            male_reps = 435 - n()) %>%
  # Pivot our dataset longer so we have number of reps in one column
  # instead of two
  pivot_longer(cols = contains("reps"),
               names_to = "gender",
               values_to = "politicians") %>%
  # Clean up the gender column since it has "_reps" at the end of it
  mutate(gender = str_remove(gender, pattern = "_.*"),
         # Make the first letter upper case
         gender = str_to_title(gender),
         # Make it a factor
         gender = factor(gender, levels = c("Male", "Female")),
         # Let's create one column to identify this dataset as Senate
         branch = "U.S. House of Representatives") %>%
  print()

# Bind our two datasets together
congress_composition <- bind_rows(house_composition, senate_composition)

# run our visualization
ggplot(congress_composition, aes(x = year, y = politicians, fill = gender)) +
  # Let's make it a column graph and change the color
  geom_area(alpha = .9, color = "gray") +
  # Let's create separate graphs for House vs. Senate
  facet_wrap(~ branch, scales = "free_y") +
  # Change the theme to classic
  theme_classic() +
  # Change the colors we're working with
  scale_fill_manual(name = "Gender", values = c("gray", "slateblue")) +
  # Let's change the names of the axes and title
  xlab("Year") +
  ylab("Number of Congress Members") +
  labs(title = "Number of U.S. Congress Members by Gender",
       subtitle = paste("Data ranges from", min(senate_composition$year), "to", max(senate_composition$year)),
       caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", face = "italic", size = 10))

# From this, we can see that in both chambers of Congress, the share of
# women has increased steadily, with a significant bump in the 1990s and a
# steady increase from there. The next question that comes to my head is:
#   When will we achieve a 50-50 parity in each chamber of Congress?
# To figure this out, I'll use an ARIMA time-series model to project out
# the trend in each chamber and figure out when the House will hit 218 women
# and the Senate 50 women.
