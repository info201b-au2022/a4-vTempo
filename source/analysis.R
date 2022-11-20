library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

# Load data frame ----
df <- read.csv("../data/incarceration_trends.csv")
state_name_code <- read.csv("../source/state_names_and_codes.csv")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Summary Information
#----------------------------------------------------------------------------#

# data frame with important values
key_df <- df %>%
  filter(year == 1978 | year == 1988 | year == 1998 | year == 2008 | year == 2018) %>%
  group_by(year) %>%
  summarize(
    tot_jailrate = round(mean(total_jail_pop_rate, na.rm = TRUE), 0),
    white_jailrate = round(mean(white_jail_pop_rate, na.rm = TRUE), 0),
    black_jailrate = round(mean(black_jail_pop_rate, na.rm = TRUE), 0),
    latinx_jailrate = round(mean(latinx_jail_pop_rate, na.rm = TRUE), 0),
    aapi_jailrate = round(mean(aapi_jail_pop_rate, na.rm = TRUE), 0),
    native_jailrate = round(mean(native_jail_pop_rate, na.rm = TRUE), 0)
  )
  
# list of important values
sum_info <- list()

# total jail rate in 1978
sum_info$tot_jr_1978 <- key_df %>%
  filter(year == 1978) %>%
  pull(tot_jailrate)

#total jail rate in 2018
sum_info$tot_jr_2018 <- key_df %>%
  filter(year == 2018) %>%
  pull(tot_jailrate)

# white jail rate in 2018
sum_info$white_jr_2018 <- key_df %>%
  filter(year == 2018) %>%
  pull(white_jailrate)

# black jail rate in 2018
sum_info$black_jr_2018 <- key_df %>%
  filter(year == 2018) %>%
  pull(black_jailrate)

#----------------------------------------------------------------------------#


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#

# This function returns a data frame with the jail population of each year
get_year_jail_pop <- function() {
  temp_df <- df %>%
    group_by(year) %>%
    summarize(jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(temp_df)
}

# This function creates a bar chart and returns it
plot_jail_pop_for_us <- function() {
  plot_jp <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(
      x = year,
      y = jail_pop)
    ) +
    labs(
      title = "Increase of Jail Population in U.S. (1970-2018)",
      caption = "Total Jail Populations More Than Quadrupled Since 1970",
      x = "Year",
      y = "Total Jail Population"
    ) +
    scale_y_continuous(labels = scales::comma)
  return(plot_jp)
}

#----------------------------------------------------------------------------#


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
#----------------------------------------------------------------------------#

# This function returns a data frame with the jail population of each year of selected states
get_jail_pop_by_states <- function(states) {
  temp_df <- df %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(temp_df)
}

# This function creates a line chart and returns it
plot_jail_pop_by_states <- function(states) {
  plot_jpbs <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(
      x = year,
      y = jail_pop,
      color = state)
    ) +
    labs(
      title = "Increase of Jail Population in U.S. States (1970-2018)",
      caption = "Each Line Corresponds to Jail Population of a State",
      x = "Year",
      y = "Total Jail Population"
    ) +
    scale_y_continuous(labels = scales::comma)
  return(plot_jpbs)
}

#----------------------------------------------------------------------------#


## Section 5  ---- 
#----------------------------------------------------------------------------#
# Black Population Proportion vs Jail Rate by State
#----------------------------------------------------------------------------#

# This function returns a data frame useful to be graphed
get_bpp_vs_jr <- function() {
  temp_df <- df %>%
    filter(year == 2018) %>%
    group_by(state) %>%
    summarize(
      bpp = sum(black_pop_15to64, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE),
      jailrate = mean(total_jail_pop_rate, na.rm = TRUE)) %>%
    select(state, bpp, jailrate)
  return(temp_df)
}

# This function creates a scatter plot and returns it
plot_bpp_vs_jr <- function() {
  plot_bpp_jr <- ggplot(get_bpp_vs_jr()) +
    geom_point(mapping = aes(
      x = bpp,
      y = jailrate)
    ) +
    labs(
      title = "Proportion of Black Population vs Jail Rate in 2018",
      caption = "Each Dot Represents A State",
      x = "Proportion of Black People in Population",
      y = "Jail Population Rate"
    )
  return(plot_bpp_jr)
}
#----------------------------------------------------------------------------#


## Section 6  ---- 
#----------------------------------------------------------------------------#
# Proportion of Inmates Being Black by State
#----------------------------------------------------------------------------#

# This function returns a data frame useful for the map
data_map <- function() {
  df_2 <- df %>%
    filter(year == 2018) %>%
    group_by(state) %>%
    summarize(
      black_jail_pop_prop =
        sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE)) %>%
    rename(Code = state) %>%
    left_join(state_name_code) %>%
    mutate(State = tolower(State))
  
  state_shape <- map_data("state") %>%
    rename(State = region) %>%
    left_join(df_2, by = "State")
  
  return(state_shape)
}

# This function creates a map for jail population and returns it
plot_jail_map <- function() {
  the_map <- ggplot(data_map()) +
    geom_polygon(mapping = aes(
      x = long,
      y = lat,
      group = group,
      fill = black_jail_pop_prop),
      color = "white",
      size = 0.5
    ) +
    coord_map() +
    labs(
      title = "Jail Population by State",
      caption = "Darker Color = Lower Proportion",
      fill = "Black Jail Proportion") +
    
    # Minimalist Theme for Map
    theme_bw() +
    theme(
      axis.line = element_blank(),        # remove axis lines
      axis.text = element_blank(),        # remove axis labels
      axis.ticks = element_blank(),       # remove axis ticks
      axis.title = element_blank(),       # remove axis titles
      plot.background = element_blank(),  # remove gray background
      panel.grid.major = element_blank(), # remove major grid lines
      panel.grid.minor = element_blank(), # remove minor grid lines
      panel.border = element_blank()      # remove border around plot
    )
  return(the_map)
}
#----------------------------------------------------------------------------#