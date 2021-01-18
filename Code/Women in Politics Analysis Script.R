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
suppressMessages(library("pacman"))
pacman::p_load("tidyverse", # Used for data wrangling,
               "tidyr", # Used for data cleaning,
               "ggplot2", # Used for visualizations,
               "maps", # Used for map-based visualizations
               "readxl", # Used for loading excel files,
               "readr", # Used for working with files,
               "pander", # Used for pretty tables,
               "kableExtra", # Used for RMarkdown formatting
               "lubridate", # Used for fixing dates,
               "praise", # Used for positive reinforcement,
               "janitor", # Used for data cleaning,
               "pdftools", # Used for reading PDF files in,
               "gganimate", # Used for interactive graphic visualizations,
               "mapproj", # Used for visualizing maps
               "transformr", # Used to animate maps
               "gifski", # Used to create animated gifs
               "forecast", # Used for time series analysis,
               "tseries")  # Used for time series analysis


# Bring in the data, taking advantage of the project structure
# Our base dataset
female_politicians_data <- readr::read_csv(here::here("Data/women_in_politics.csv"))
census_data <- readr::read_csv(here::here("Data/Census_Structured.csv"))

# Convert to a tibble, my preferred data structure
(female_politicians_data <- as_tibble(female_politicians_data))


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
         years_of_service = max(year) - min(year),
         years_in_office = str_c(min(year), max(year), sep = "-")) %>%
  # Reorder columns to see the result
  select(id, contains("year"), contains("name"), everything()) %>%
  ungroup() %>%
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

# Total number of Female Politicians holding office by Level and party
(Women_in_Office_by_Party <- wp_selected %>%
  # Select our variables to analyze
  select(id, level, party_grouped) %>%
  # Pull only distinct values
  distinct() %>%
  # Group by level and political party
  group_by(level, party_grouped) %>%
  # Count everything up!
  summarise(num = n()) %>%
  ungroup() %>%
  # Bring our colors back in
  left_join(party_colors, by = "party_grouped") %>%
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
    labs(title = "Number of Female Politicians by Party",
         subtitle = paste("Data, broken out by various levels of government, ranges from", min(wp_selected$year), "to", max(wp_selected$year)),
         caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
    # format our title and subtitle
    theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
          plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
          plot.caption = element_text(color = "dark gray", size = 10, face = "italic"))
  )
ggsave(here::here("Viz", "Women_in_Office_by_Party.jpg"))

# Quite clearly, most of the women who have held office have done so at the state legislative
# level. Interestingly enough, most of the women who have held office at the state
# legislative or Federal/Congress level have been members of the two major parties.
# In D.C., a lot of women outside the two major parties have won office.

# This is great to see total numbers, but our data spans nearly 130 years!
# Let's breakout our totals over time to look for trends.

# Let's break out these numbers of Female Politicians holding office over time
(Women_in_Office_Over_Time <- wp_selected %>%
    # Select our variables to analyze
    select(id, level, year) %>%
    # Pull only distinct values
    distinct() %>%
    # Group by level and political party
    group_by(year, level) %>%
    # Count everything up!
    summarise(num = n()) %>%
    # Because it'll skew the data, let's get rid of anything from the current
    # year or later
    filter(year < year(Sys.Date())) %>%
    # Start our visualization, creating our groups by party affiliation
    ggplot(aes(x = year, y = num)) +
    geom_line(lwd = 1.5, color = "slateblue") +
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
)
ggsave(here::here("Viz", "Women_in_Office_Over_Time.jpg"))

# Here are some key takeaways:
#   1. The number of women holding elected office has been growing incredibly
#      fast over the past few decades
#   2. Most of the positions held are at the State Legislative level, which
#      makes sense given the higher number of positions open at that level.

# Let's break out these numbers of Female Politicians holding office over time by party
(Women_in_Office_Over_Time_Party <- wp_selected %>%
  # Select our variables to analyze
  select(id, level, party_grouped, year) %>%
  # Pull only distinct values
  distinct() %>%
  # Group by level and political party
  group_by(year, level, party_grouped) %>%
  # Count everything up!
  summarise(num = n()) %>%
  # Bring our colors back in
  left_join(party_colors, by = "party_grouped") %>%
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
  labs(title = "Number of Female Politicians by Party",
       subtitle = paste("Data ranges from", min(wp_selected$year), "to", max(wp_selected$year)),
       caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic"))
)
ggsave(here::here("Viz", "Women_in_Office_Over_Time_Party.jpg"))

# For female politicians in Congress, Democrats and Repulicans held
# lock-step until 1990, when Democrats took off at a much faster rate.

### Women in Office by Race
# How does this change not by political party, but by race? Let's take a look.
# Let's break out these numbers of Female Politicians holding office over time by party
(Women_in_Office_Over_Time_Race <- wp_selected %>%
    # We actually have a facet of our data that race/ethnicity is broken out
    # by extremely specific categories, which overwhelms the data viz. Let's
    # combine any woman who is multiracial into the same cateogry
    mutate(new = case_when(
      str_detect(race_ethnicity, "Multiracial") ~ TRUE,
      TRUE ~ FALSE),
      race_ethnicity = if_else(new == T, "Multiracial", race_ethnicity),
      ) %>%
    # Select our variables to analyze
    select(id, level, race_ethnicity, year) %>%
    # Drop any data that is unavailable
    filter(race_ethnicity != "Unavailable") %>%
    # Pull only distinct values
    distinct() %>%
    # Group by level and political party
    group_by(year, level, race_ethnicity) %>%
    # Count everything up!
    summarise(num = n()) %>%
    # Because it'll skew the data, let's get rid of anything from the current
    # year or later
    filter(year < year(Sys.Date())) %>%
    # Start our visualization, creating our groups by party affiliation
    ggplot(aes(x = year, y = num, color = race_ethnicity)) +
    geom_line(lwd = 2) +
    # Change our color scales
    scale_color_brewer(name = "Race", palette = "Set1") +
    # Create a separate chart, with a flexible y-axis, for each level of office
    facet_wrap(~level, scales = "free_y") +
    # Change the theme to classic
    theme_classic() +
    # Let's change the names of the axes and title
    xlab("Year") +
    ylab("Number of Female Politicians") +
    labs(title = "Number of Female Politicians by Race",
         subtitle = paste("Data ranges from", min(wp_selected$year), "to", max(wp_selected$year)),
         caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
    # format our title and subtitle
    theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
          plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
          plot.caption = element_text(color = "dark gray", size = 10, face = "italic"))
)
ggsave(here::here("Viz", "Women_in_Office_Over_Time_Race.jpg"))

### Find the average number of years of service at each level of government
mu <- plyr::ddply(wp_selected, "level", summarise, years_mean = mean(years_of_service))

# How long have women served, on average, at various levels of government?
(Years_of_Service <- wp_selected %>%
    # Select our variables to analyze
    select(id, level, years_of_service) %>%
    # Pull only distinct values
    distinct() %>%
    # Start our visualization, creating our groups by party affiliation
    ggplot(aes(x = years_of_service, color = level, fill = level)) +
    # Use a density plot to better see the distribution of years_of_service
    geom_density(alpha = .1, lwd = 1) +
    # Add our average lines, calculated above using ddply
    geom_vline(data = mu,
               aes(xintercept = years_mean,
                   color = level),
               linetype = "dashed") +
    # Change the theme to classic
    theme_classic() +
    # Change our color scales
    scale_color_brewer(palette = "Set1") +
    # Let's change the names of the axes and title
    labs(title = "Years of Service of Female Politicians",
         subtitle = paste("Data, broken out by various levels of government, ranges from", min(wp_selected$year), "to", max(wp_selected$year)),
         x = "Years of Service",
         y = "Number of Female Politicians",
         # Change the title of the legend by specifying both the color/fill aesthetics of the viz
         color = "Level of Government",
         fill = "Level of Government",
         caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
    # format our title and subtitle
    theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
          plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
          plot.caption = element_text(color = "dark gray", size = 10, face = "italic"))
)
ggsave(here::here("Viz", "Years_of_Service.jpg"))

### Congress
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
         chamber = "U.S. Senate") %>%
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
         chamber = "U.S. House of Representatives") %>%
  print()

# Bind our two datasets together
congress_composition <- bind_rows(house_composition, senate_composition)

# run our visualization
(US_Congressmembers_by_Gender <- ggplot(congress_composition, aes(x = year, y = politicians, fill = gender)) +
  # Let's make it a column graph and change the color
  geom_area(alpha = .9, color = "gray") +
  # Let's create separate graphs for House vs. Senate
  facet_wrap(~ chamber, scales = "free_y") +
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
  )
ggsave(here::here("Viz", "US_Congressmembers_by_Gender.jpg"))

# From this, we can see that in both chambers of Congress, the share of
# women has increased steadily, with a significant bump in the 1990s and a
# steady increase from there. The next question that comes to my head is:
#   When will we achieve a 50-50 parity in each chamber of Congress?
# To figure this out, I'll use an ARIMA time-series model to project out
# the trend in each chamber and figure out when the House will hit 218 women
# and the Senate 50 women.


########################################################################
## ARIMA ---------------------------------------------------------------
########################################################################
# In this next section, I'd like to use an ARIMA time forecasting model to
# determine when each chamber of the U.S. Congress will achieve full gender
# parity. What does that mean in plain English? Can we use data science methods
# to determine when the Senate will have 50 female members and the House 217
# (435/2) female members?

senate_dates <- congress_composition %>%
  filter(gender == "Female") %>%
  filter(chamber == "U.S. Senate") %>%
  print()

# There are a bunch of years that don't have any female politicians, so
# let's impute the value 0 for them. Start by determining what the total
# number of years should look like
senate_full <- senate_dates %>%
  tidyr::complete(year = min(senate_dates$year):max(senate_dates$year),
                  fill = list(gender = "Female",
                              politicians = 0,
                              chamber = "U.S. Senate"
                  )
  ) %>%
  # Create a date column out of the year variable
  mutate(date = as.Date(paste(year, "01", "01", sep = "-")),
         period = "historic") %>%
  print()


# Create a time series object from our date values. We'll start our time
# series object from the first year (column) in our data frame and end at
# the final year in our dataset.
senate_ts <- ts(data = senate_full$politicians,
                start = min(senate_full$year),
                end = max(senate_full$year))

# Use auto.arima to build a regression model with ARIMA
arima_senate <- auto.arima(senate_ts)

# Let's build out our dataset 81 years into the future to forecast any increase
num_years <- 81
# Use the forecast function to build our predictions
senate_forecast <- forecast(arima_senate,
                            h = num_years # Number of years to forecast
                            )

# Let's structure these forecasted values in the same form as our dataset,
# and add an indicator variable in
forecast_full <- tibble(
  politicians = senate_forecast$mean,
  year = seq(max(senate_full$year) + 1, max(senate_full$year) + num_years, 1),
  gender = "Female",
  chamber = "U.S. Senate",
  date = as.Date(paste(year, "01", "01", sep = "-")),
  period = "future" # indicator variable
)

senate_future <- senate_full %>%
  bind_rows(forecast_full) %>%
  print()

# Thus, according to the Senate ARIMA model built, the U.S. Senate will first
# achieve full gender parity in the year:
(senate_parity <- min(senate_future$year[senate_future$politicians > 50]))


# Let's plot our data
(Senate_ARIMA_Viz <- ggplot(senate_future, aes(x = year, y = politicians, color = period)) +
  geom_line(lwd = 2) +
  # Add a reference line for when we achieve full parity
  geom_vline(xintercept = senate_parity,
             color = "slateblue1",
             linetype = "dashed") +
  geom_hline(yintercept = 50,
             color = "slateblue1",
             linetype = "dashed") +
  # Change the theme to classic
  theme_classic() +
  # Change the colors we're working with
  scale_color_manual(name = "Time Period",
                     values = c("slateblue", "gray"),
                     labels = c("Predicted", "Actual")) +
  # Let's change the names of the axes and title
  xlab("Year") +
  ylab("Number of Senators") +
  labs(title = "Number of Female U.S. Senators over Time",
       subtitle = paste("Data ranges from ", min(senate_future$year), " to ", max(senate_future$year), ". The dashed lines represent\nthe year in which each chamber achieves full gender parity.", sep = ""),
       caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", face = "italic", size = 10))
)
ggsave(here::here("Viz", "Senate_ARIMA_Viz.jpg"))


## Let's now do the same thing for the House
house_dates <- congress_composition %>%
  filter(gender == "Female") %>%
  filter(chamber == "U.S. House of Representatives") %>%
  print()

# There are a bunch of years that don't have any female politicians, so
# let's impute the value 0 for them. Start by determining what the total
# number of years should look like
house_full <- house_dates %>%
  tidyr::complete(year = min(house_dates$year):max(house_dates$year),
                  fill = list(gender = "Female",
                              politicians = 0,
                              chamber = "U.S. House of Representatives"
                  )
  ) %>%
  # Create a date column out of the year variable
  mutate(date = as.Date(paste(year, "01", "01", sep = "-")),
         period = "historic") %>%
  print()


# Create a time series object from our date values. We'll start our time
# series object from the first year (column) in our data frame and end at
# the final year in our dataset.
house_ts <- ts(data = house_full$politicians,
                start = min(house_full$year),
                end = max(house_full$year))

# Use auto.arima to build a regression model with ARIMA
arima_house <- auto.arima(house_ts)


# Use the forecast function to build our predictions
house_forecast <- forecast(arima_house,
                            h = num_years # Number of years to forecast
                           )

# Let's structure these forecasted values in the same form as our dataset,
# and add an indicator variable in
forecast_full <- tibble(
  politicians = house_forecast$mean,
  year = seq(max(house_full$year) + 1, max(house_full$year) + num_years, 1),
  gender = "Female",
  chamber = "U.S. House of Representatives",
  date = as.Date(paste(year, "01", "01", sep = "-")),
  period = "future" # indicator variable
)

house_future <- house_full %>%
  bind_rows(forecast_full) %>%
  print()

# Thus, according to the House ARIMA model built, the U.S. House of
# Representatives will first achieve full gender parity in the year:
(house_parity <- min(house_future$year[house_future$politicians > 435/2]))

# Let's plot our data
(House_ARIMA_Viz <- ggplot(house_future, aes(x = year, y = politicians, color = period)) +
  geom_line(lwd = 2) +
  # Add a reference line for when we achieve full parity
  geom_vline(xintercept = house_parity,
             color = "slateblue1",
             linetype = "dashed") +
  geom_hline(yintercept = 435/2,
             color = "slateblue1",
             linetype = "dashed") +
  # Change the theme to classic
  theme_classic() +
  # Change the colors we're working with
  scale_color_manual(name = "Time Period",
                     values = c("slateblue", "gray"),
                     labels = c("Predicted", "Actual")) +
  # Let's change the names of the axes and title
  xlab("Year") +
  ylab("Number of Congresswomen") +
  labs(title = "Number of Female U.S. House Reps over Time",
       subtitle = paste("Data ranges from ", min(house_future$year), " to ", max(house_future$year), ". The dashed lines represent\nthe year in which each chamber achieves full gender parity.", sep = ""),
       caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", face = "italic", size = 10))
)
ggsave(here::here("Viz", "House_ARIMA_Viz.jpg"))


# Let's plot the Senate and House graphs side-by-side
# Start by bringing the datasets together and imputing a new variable called
# chamber
congress_future <- house_future %>%
  mutate(chamber = "U.S. House of Representatives",
         parity_year = house_parity,
         parity_number = 435/2) %>%
  bind_rows(senate_future) %>%
  mutate(chamber = if_else(is.na(chamber), "U.S. Senate", chamber),
         parity_year = if_else(is.na(parity_year), senate_parity, parity_year),
         parity_number = if_else(is.na(parity_number), 50, parity_number)) %>%
  arrange(year) %>%
  print()



# Let's plot our data
(Congress_ARIMA_Viz <- ggplot(congress_future, aes(x = year, y = politicians, color = period)) +
  geom_line(lwd = 1.5) +
  # Facet wrap to get two graphs
  facet_wrap(~ chamber, scales = "free_y") +
  # Add a reference line for when we achieve full parity
  geom_vline(data = congress_future,
             aes(xintercept = parity_year),
             color = "slateblue1",
             linetype = "dashed") +
  geom_hline(data = congress_future,
             aes(yintercept = parity_number),
             color = "slateblue1",
             linetype = "dashed") +
  # Change the theme to classic
  theme_classic() +
  # Change the colors we're working with
  scale_color_manual(name = "Time Period",
                     values = c("slateblue", "gray"),
                     labels = c("Predicted", "Actual")) +
  # Let's change the names of the axes and title
  xlab("Year") +
  ylab("Number of Congresswomen") +
  labs(title = "Number of U.S. Congresswomen over Time",
       subtitle = paste("Data ranges from ", min(house_future$year), " to ", max(house_future$year), ". The dashed lines represent\nthe year in which each chamber achieves full gender parity.", sep = ""),
       caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", face = "italic", size = 10))
)
ggsave(here::here("Viz", "Congress_ARIMA_Viz.jpg"))


# From these graphs, we can see that both ARIMA models took a rather linear
# approach, assuming a steady increase over time. This makes sense, because
# ARIMA usually looks for seasonality trends (which our data does NOT have)
# on top of the general trends (which our data does have). Obviously, this
# assumes that the rate of increase is steady over time and doesn't plateau
# as women in Congress hit a certain threshold. Assuming the rate of increase
# is generally linear, the Senate will achieve full parity before the House.


########################################################################
## Logistic Model ------------------------------------------------------
########################################################################
# Because a linear approach is unrealistic (eventually, the threshold of women
# in each chamber will eventually exceed 100%) in the long run, let's try to fit
# something that caps out at 100%. For that reason, I'd like to fit a logistic
# regression curve, which uses a Sigmoidal function (S-shaped) to make its estimate.
p_load(drc)

# If we just go ahead and fit the logistic regression based on what we have now,
# It won't put the upper bound at 100% female. Let's make a daring assumption
# that Congress will eventually be 100% female.
house_100 <- tibble(
  year = max(house_full$year) + num_years,
  politicians = 435
) %>%
  bind_rows(house_full) %>%
  arrange(year) %>%
  print()

log_fit_house <- drm(politicians ~ year, data = house_100, fct = L.3(), type = "continuous")
pander(summary(log_fit))

# Predict out our future values for 100 years out!
new_house_data <- expand.grid(year = seq(max(house_full$year) + 1, max(house_full$year) + num_years - 1))
log_house_predictions <- predict(log_fit, newdata = new_house_data)

# Bring our results (first column) in
log_house_full <- new_house_data %>%
  as_tibble() %>%
  bind_cols(politicians = log_house_predictions) %>%
  mutate(period = "future") %>%
  bind_rows(house_100) %>%
  arrange(year) %>%
  tidyr::fill(gender, chamber) %>%
  mutate(date = as.Date(paste(year, "01", "01", sep = "-"))) %>%
  print()


# Now let's do the same thing for the Senate
senate_100 <- tibble(
  year = max(senate_full$year) + num_years,
  politicians = 100
) %>%
  bind_rows(senate_full) %>%
  arrange(year) %>%
  print()

log_fit_senate <- drm(politicians ~ year, data = senate_100, fct = L.3(), type = "continuous")
pander(summary(log_fit_senate))

# Predict out our future values for 100 years out!
new_senate_data <- expand.grid(year = seq(max(senate_full$year) + 1, max(senate_full$year) + num_years - 1))
log_senate_predictions <- predict(log_fit_senate, newdata = new_senate_data)

# Bring our results (first column) in
log_senate_full <- new_senate_data %>%
  as_tibble() %>%
  bind_cols(politicians = log_senate_predictions) %>%
  mutate(period = "future") %>%
  bind_rows(senate_100) %>%
  arrange(year) %>%
  tidyr::fill(gender, chamber) %>%
  mutate(date = as.Date(paste(year, "01", "01", sep = "-"))) %>%
  print()

log_congress_full <- log_house_full %>%
  bind_rows(log_senate_full) %>%
  mutate(parity_year = if_else(chamber == "U.S. House of Representatives",
                                   min(log_house_full$year[log_house_full$politicians > 435/2]),
                                   min(log_senate_full$year[log_senate_full$politicians > 100/2])),
         parity_number = if_else(chamber == "U.S. House of Representatives",
                               min(log_house_full$politicians[log_house_full$politicians > 435/2]),
                               min(log_senate_full$politicians[log_senate_full$politicians > 100/2])),
         model = "Logistic"
         )

# Viz time!
log_congress_full %>%
  ggplot(aes(x = year, y = politicians, color = period)) +
  geom_line(lwd = 1.5) +
  facet_wrap(~ chamber, scales = "free_y") +
  # Add a reference line for when we achieve full parity
  geom_vline(data = log_congress_full,
             aes(xintercept = parity_year),
             color = "slateblue1",
             linetype = "dashed") +
  geom_hline(data = log_congress_full,
             aes(yintercept = parity_number),
             color = "slateblue1",
             linetype = "dashed") +
  # Change the theme to classic
  theme_classic() +
  # Change the colors we're working with
  scale_color_manual(name = "Time Period",
                     values = c("gray", "slateblue"),
                     labels = c("Actual", "Predicted")) +
  # Let's change the names of the axes and title
  xlab("Year") +
  ylab("Number of Congresswomen") +
  labs(title = "Number of U.S. Congresswomen over Time",
       subtitle = paste("Data ranges from ", min(log_senate_full$year), " to ", max(log_senate_full$year), ". The dashed lines represent\nthe year in which each chamber achieves full gender parity.", sep = ""),
       caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", face = "italic", size = 10))


# Let's see what this looks like against the ARIMA estimates we made
both_models_full <- log_congress_full %>%
  bind_rows(congress_future) %>%
  mutate(model = if_else(is.na(model), "ARIMA", model))

both_models_full %>%
  # log_senate_full %>%
  ggplot(aes(x = year, y = politicians, color = period)) +
  geom_line(lwd = 1.5) +
  facet_wrap(~ model + chamber, scales = "free_y") +
  # Add a reference line for when we achieve full parity
  geom_vline(data = both_models_full,
             aes(xintercept = parity_year),
             color = "slateblue1",
             linetype = "dashed") +
  geom_hline(data = both_models_full,
             aes(yintercept = parity_number),
             color = "slateblue1",
             linetype = "dashed") +
  # Change the theme to classic
  theme_classic() +
  # Change the colors we're working with
  scale_color_manual(name = "Time Period",
                     values = c("slateblue", "gray"),
                     labels = c("Predicted", "Actual")) +
  # Let's change the names of the axes and title
  xlab("Year") +
  ylab("Number of Congresswomen") +
  labs(title = "Number of U.S. Congresswomen over Time",
       subtitle = paste("Data ranges from ", min(both_models_full$year), " to ", max(both_models_full$year), ". The dashed lines represent\nthe year in which each chamber achieves full gender parity.", sep = ""),
       caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", face = "italic", size = 10))



########################################################################
## Geographic Analysis -------------------------------------------------
########################################################################
# Next, we'll look into how the number of women holding certain elected
# office varies based on location (state). This will help us understand
# if there are certain areas of the country where women holding office
# are taking off, and others where it's lagging.

# First, let's take a look at our all-time leaders. Which states have had
# the most women representatives/executives in the past 120 years?
wp_state_sums <- wp_selected %>%
  # Get rid of D.C. for now
  filter(level != "Territorial/D.C.") %>%
  # Select our variables to analyze
  select(id, level, state) %>%
  # Group by level and state
  group_by(level, state) %>%
  # Count everything up!
  summarise(num = n())

(Women_in_Office_by_State <- wp_state_sums %>%
   # Pick our top 10
   top_n(10, num) %>%
   # Rearrange our dataset
   arrange(level, desc(num)) %>%
   # Start our visualization, creating our groups by party affiliation
   ggplot(aes(x = num, y = factor(num))) +
   geom_bar(stat = "identity", fill = "slateblue", na.rm = T) +
   # Create a separate chart, with a flexible y-axis, for each level of office
   facet_wrap(~level, scales = "free_y") +
   # Add a label by recreating our data build from earlier
   geom_label(data = wp_selected %>%
                # Get rid of D.C. for now
                filter(level != "Territorial/D.C.") %>%
                # Select our variables to analyze
                select(id, level, state_abb) %>%
                # Group by level and state
                group_by(level, state_abb) %>%
                # Count everything up!
                summarise(num = n()) %>%
                # Pick our top 10
                top_n(10, num) %>%
                # Rearrange our dataset
                arrange(level, desc(num)),
              aes(label = paste(state_abb, num, sep = "-")),
              size = 3,
              # Scooch the labels over a smidge
              hjust = .25) +
   # Change the theme to classic
   theme_classic() +
   # Let's change the names of the axes and title
   xlab("Party") +
   ylab("Number of Female Politicians") +
   labs(title = "Number of Female Politicians at Various\nLevels of Government by State",
        subtitle = paste("Data ranges from", min(wp_selected$year), "to", max(wp_selected$year)),
        caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/") +
   # format our title and subtitle
   theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
         plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
         plot.caption = element_text(color = "dark gray", size = 10, face = "italic"),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank())
)
ggsave(here::here("Viz", "Women_in_Office_by_State.jpg"))


# From this we can see that certain states have more women in Congress than others.
# This isn't wholly helpful because certain states that dominate at the Federal
# level (like California, New York, and Florida), generally have *more*
# representatives compared to smaller states (like Rhode Island, Wyoming, and
# Delaware). This visualization would be much more representative if I were
# to normalize by state population.



wp_state_yearly <- wp_selected %>%
  # Get rid of D.C. for now
  filter(grepl("State Legislative", level)) %>%
  # Select our variables to analyze
  select(id, level, state, year) %>%
  # Group by level and state
  group_by(level, state, year) %>%
  # Count everything up!
  summarise(num = n())

wp_state_filled <- wp_state_yearly %>%
  # Fill in missing data because there are a lot of years where a state had
  # no female representatives, so those rows are just "missing" from the
  # dataset, whereas I'd like them to display a '0'
  tidyr::complete(year = min(wp_state_yearly$year):max(wp_state_yearly$year),
                  nesting(level, state),
                  fill = list(num = 0)
  ) %>%
  # Bring in our population data
  left_join(census_data, by = c("year", "state")) %>%
  # Get rid of anything before 1900 since we don't have data for that or
  # anything after 2019
  filter(year >= 1900 & year < 2020) %>%
  # Create a new column that normalizes population and multiplies by 1000000
  # Because some states have 0 population, deal with these accordingly
  mutate(num_normalized = if_else(population == 0, 0, 1000000*num/population))

# Now create a dataframe that holds the top 10 states and their values from every year
top_10_by_state <- wp_state_filled %>%
  group_by(year) %>%
  mutate(rank = 1.0 * rank(-num_normalized),
         num_normalized_rel = num_normalized/num_normalized[rank == 1],
         num_label = paste0(" ", round(1.0 * num_normalized, 1))) %>%
  group_by(state) %>%
  filter(rank <= 10) %>%
  ungroup() %>%
  # Resort our dataset by year and num_normalized
  arrange(year, desc(num_normalized))


# Start by saving all of our graphs into one object
static_bar_plot <- top_10_by_state %>%
  ggplot(
    aes(rank,
        group = state_code)
    ) +
  geom_tile(aes(y = num_normalized/2,
                height = num_normalized,
                width = 0.9),
            fill = "slateblue") +
  # Create our labels for states, our x-axis
  geom_text(aes(y = 0,
                label = paste(state_code, " ")),
            vjust = 0.2,
            hjust = 1,
            size = 4) +
  # Create our labels for values
  geom_label(aes(y = num_normalized,
                 label = num_label),
             hjust = 0,
             size = 4) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  # Remove all of the theming so the plot looks simple enough
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic"),
        plot.background=element_blank(),
        plot.margin = margin(2, 2, 2, 4, "cm"))

# Now use gganimate to stitch together all of the static plots
animated_bar_plot <- static_bar_plot +
  # Split by a discrete variable, year, and animate between the different states
  transition_states(states = year,
                    transition_length = 4,
                    state_length = 2) +
  # Provides a view as if the background lines are moving as the animation is progressing
  view_follow(fixed_x = T,
              fixed_y = T) +
  labs(title = "Top 10 States by Number of Female State Legislators\nYear: {closest_state}",
       subtitle = "Data is normalized by state per 1,000,000 Population (i.e. 50 means\nfor every 1,000,000 people, there are 50 women who were State Legislators",
       caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/"
       )


# Now render the plot
gganimate::animate(
  plot = animated_bar_plot,
  # A larger number of frames seems to slow down our viz
  nframes = 600,
  fps = 20,
  width = 800,
  height = 600,
  render = gifski_renderer("Viz/Top 10 States Animation.gif"))


### Map visualization
# Read in the states data, which includes the longitude/latitude points I need
main_states <- map_data("state")
# Change the structure of the main_states object
main_states <- main_states %>%
  as_tibble() %>%
  # Make all of the states lower case to match
  mutate(region = str_to_title(region)) %>%
  # Rename state variable to match what's in the other datasets, for joining purposes
  rename(state = region) %>%
  print()

# Join our state longitude/latitude data into our main dataframe
wp_state_joined <- wp_state_filled %>%
  merge(main_states, by = "state") %>%
  as_tibble()


### Animated maps
# Test map
# ggplot(data = main_states,
#             mapping = aes(x = long, y = lat,
#                           group = group, fill = state)) +
#   geom_polygon(color = "gray90", size = 0.1) +
#   coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
#   guides(fill = FALSE) +
#   theme_void()
#
# wp_2015 <- wp_state_joined %>%
#   as_tibble() %>%
#   filter(year > 1990)

# Let's turn our bar charts into maps. We'll use our original data frame so
# get more than the top 10 states (ideally we get all)
state_map <-  ggplot(data = wp_state_joined,
                     aes(x = long,
                         y = lat)) +
  geom_polygon(aes(group = group,
                   fill = num_normalized),
               color = "black") +
  # Note: We can only use the albers projection if we're looking at just the U.S.
  # If we wanted to expand across the Atlantic, we'd have to use Mercator
  coord_map(projection = "albers", lat0 = 45.5, lat1 = 29.5) +
  guides(fill = FALSE) +
  # Remove all of the theming so the plot looks simple enough
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        # legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic"),
        plot.background=element_blank()) +
  # Change the fill scale
  guides(fill = guide_legend(title = "Scale", reverse = T)) +
  scale_fill_continuous(low = "white",
                        high = "slateblue",
                        limits = c(0, 100))

# Now use gganimate to stitch together all of the static plots
animated_map_plot <- state_map +
  # Split by a discrete variable, year, and animate between the different states
  transition_states(states = year,
                    transition_length = 4,
                    state_length = 2) +
  labs(title = "Number of Female State Legislators by State\nYear: {closest_state}",
       subtitle = "Data is normalized by state per 1,000,000 Population (i.e. 50 means for every\n1,000,000 people, there are 50 women who were State Legislators",
       caption = "Data is gathered from the Eagleton Institute of Politics,\nCenter for American Women in Politics at\nhttps://cawpdata.rutgers.edu/"
  )

# Now render the plot
gganimate::animate(
  plot = animated_map_plot,
  # A larger number of frames seems to slow down our viz
  nframes = 600,
  fps = 20,
  width = 800,
  height = 600,
  render = gifski_renderer("Viz/States Map Animation.gif"))


