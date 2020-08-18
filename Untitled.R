library(dplyr)

library(tidyverse)

# COVID Data

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'

covid = read.csv(url)


covid %>%
  filter(date == max(date)) %>%
  group_by(state) %>%
  summarize(case = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  slice_max(case, n = 6)


#->
 # new_covid

# most cumulative cases
new_covid %>%
  filter(date == max(date)) %>%
  select("county", "cases") %>%
  slice_max(cases, n = 5)
#  most_cumulative_cases
