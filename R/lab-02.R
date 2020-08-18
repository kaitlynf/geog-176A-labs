install.packages("tidyverse")

install.packages("knitr")

install.packages("readxl")

install.packages("zoo")

install.packages("dplyr")

library(lubridate)

prods.all$Date2 <- mdy(prods.all$Date2)

install.packages("lubridate")

library(dplyr)

library(tidyverse)

library(zoo)

# COVID Data

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'

covid = read.csv(url)

covid %>%
  filter(state == "California") %>%
  group_by(county) %>%
  mutate(newCases = cases - lag(cases)) %>%
  filter(date == max(date)) %>%
  ungroup() ->
  new_covid

# most cumulative cases
new_covid %>%
  filter(date == max(date)) %>%
  slice_max(cases, n = 5) %>%
  select("county", "cases") ->
  most_cumulative_cases

knitr::kable(most_cumulative_cases,
             caption = "Most Cumulative Cases by California Counties",
             col.names = c("County", "Cumulative Cases"))

# most new cases
new_covid %>%
  filter(date == max(date)) %>%
  slice_max(newCases, n = 5) %>%
  select(county, newCases) ->
  most_new_cases

knitr::kable(most_new_cases,
             caption = "Most New Cases by California Counties",
             col.names = c("County", "New Cases"))

#Population Estimate Data
library(readxl)
pop <- read_excel("data/PopulationEstimates.xls",
                                  skip = 2)

pop %>%
select (fips = "FIPStxt", state = "State", "Area_Name", pop2019 = "POP_ESTIMATE_2019") %>%
  mutate(fips = as.integer(fips)) %>%
  filter(state == "CA") %>%
  group_by(Area_Name) ->
  new_pop

#Covid data and population data joined to create "covidPop"
right_join(new_covid, new_pop, by = "fips") ->
  covidPop

covidPop %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= max(date) - days(14))



covid %>%
  group_by(state, date) %>%
  summarize(cases = sum(cases)) %>%
  ungroup() %>%
  filter(state %in% c("New York", "California", "Louisiana", "Florida")) %>%
  group_by(state) %>%
  mutate(newCases = cases - lag(cases)) %>%
  mutate(avg = zoo::rollmean(newCases, 7, fill = NA, allign = "right"))

right_join(new_covid, new_pop, by = "fips")

#working for 4 states
pop %>%
  select (fips = "FIPStxt", state = "State", "Area_Name", pop2019 = "POP_ESTIMATE_2019") %>%
  mutate(fips = as.integer(fips)) %>%
  filter(state %in% c("NY","CA","LA", "FL")) ->
  new_pop4


#working for 4 states
covid %>%
  filter(state %in% c("New York", "California", "Louisiana", "Florida")) %>%
  group_by(state)%>%
  mutate(newCases = cases - lag(cases)) %>%
  ungroup() ->
  new_covid4

right_join(new_covid4, new_pop4, by = "fips") -> covidPop4

covid %>%
  group_by(state, date, fips) %>%
  summarize(cases = sum(cases)) %>%
  ungroup() %>%
  filter(state %in% c("New York", "California", "Louisiana", "Florida")) %>%
  group_by(state) %>%
  mutate(newCases = cases - lag(cases)) %>%
  mutate(avg = zoo::rollmean(newCases, 7, fill = NA, allign = "right")) ->
  covid_step1


covid %>%
  filter(state %in% c("California", "New York", "Louisiana", "Florida")) %>%
  group_by(state) %>%
  mutate(newCases = cases - lag(cases)) %>%
  mutate(avg = zoo::rollmean(newCases, 7, fill = NA, allign = "right")) %>%
  ungroup() ->
  new_covid4

pop %>%
  select (fips = "FIPStxt", state = "State", "Area_Name", pop2019 = "POP_ESTIMATE_2019") %>%
  mutate(fips = as.integer(fips)) %>%
  filter(state %in% c("NY","CA","LA", "FL")) ->
  new_pop4

inner_join(new_covid4, new_pop4, by = "fips") -> covidPop4

covidPop4 %>%
  filter(date == max(date))
