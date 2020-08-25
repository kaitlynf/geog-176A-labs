---
  title: "Geography 176A"
author: "Kaitlyn Fu"
subtitle: 'Lab 03: Distances and the Border Zone'
output:
  html_document:
  theme: journal
---


# SPDS
library(tidyverse)
library(sf)
library(units)

# Data
library(USAboundaries)
library(rnaturalearthdata)

# Visualization
library(gghighlight)
library(ggrepel)
library(knitr)

##Question 1
#continental US States
USAboundaries::us_states(resolution = "low") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  filter(!state_name %in% c("Puerto Rico",
                            "Alaska",
                            "Hawaii")) ->
  conus

#set projected coordinate system to 'eqdc'
eqdc = '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

st_transform(conus, eqdc) -> conus

#country boundaries for Mexico, the United States of America, and Canada
rnaturalearthdata::countries110 -> countries110

st_as_sf(countries110, crs = 4326) -> worldboundaries

st_transform(worldboundaries, eqdc) -> worldboundaries

#loading cities from CSV file
readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  filter(!state_name %in% c("Hawaii", "Alaska", "Puerto Rico")) %>%
         st_transform(., eqdc) ->
  uscities

##Question 2
#distance to US borders (2.1)

st_union(conus) %>%
  st_cast("MULTILINESTRING") ->
  USboundaries

uscities %>%
  select(city, state_name) %>%
  mutate(dist = st_distance(uscities, USboundaries),
  dist = units::set_units(dist, "km"),
  dist = units::drop_units(dist)) ->
  border_dist

border_dist %>%
  slice_max(dist, n = 5) %>%
  st_drop_geometry() ->
  top5_to_border

knitr::kable(top5_to_border,
             caption = "Distance of US Cities to US Borders",
             col.names = c("City", "State", "Distance to Border"))


#2.2 calculating the distance of each city to the nearest state boundary
st_combine(conus) %>%
  st_cast("MULTILINESTRING") ->
  USboundaries2

uscities %>%
  select(city, state_name) %>%
  mutate(dist2 = st_distance(uscities, USboundaries2),
         dist2 = units::set_units(dist2, "km"),
         dist2 = units::drop_units(dist2)) ->
  border_dist2

border_dist2 %>%
  slice_max(dist2, n = 5) %>%
  st_drop_geometry() ->
  top5_to_stateborder

knitr::kable(top5_to_stateborder,
             caption = "Distance of US Cities to US State Borders",
             col.names = c("City", "State", "Distance to State Border"))

#2.3 calculating the distance of each city to the Mexican Border
worldboundaries %>%
  st_cast("MULTILINESTRING") %>%
  filter(admin == "Mexico") ->
  mexico_border

uscities %>%
  select(city, state_name) %>%
  mutate(dist3 = st_distance(uscities, mexico_border),
         dist3 = units::set_units(dist3, "km"),
         dist3 = units::drop_units(dist3)) ->
  border_dist3

border_dist3 %>%
  slice_max(dist3, n = 5) %>%
  st_drop_geometry() ->
  top5_to_mexico

knitr::kable(top5_to_mexico,
             caption = "Distance of US Cities to Mexican Border",
             col.names = c("City", "State", "Distance to Mexican Border"))

#2.4 calculating the distance of each city to the Canadian border
worldboundaries %>%
  st_cast("MULTILINESTRING") %>%
  filter(admin == "Canada") ->
  canada_border

uscities %>%
  select(city, state_name) %>%
  mutate(dist4 = st_distance(uscities, canada_border),
         dist4 = units::set_units(dist4, "km"),
         dist4 = units::drop_units(dist4)) ->
  border_dist4

border_dist4 %>%
  slice_max(dist4, n = 5) %>%
  st_drop_geometry() ->
  top5_to_canada

knitr::kable(top5_to_canada,
             caption = "Distance of US Cities to Canadian Border",
             col.names = c("City", "State", "Distance to Canadian Border"))


#3.1
uscities %>%
  slice_max(population, n = 10) ->
  top10_pop

ggplot() +
  geom_sf(data = conus) +
  geom_sf(data = top10_pop$geometry, add = TRUE, size= 2, color = "red") +
  ggrepel::geom_label_repel(
    data = top10_pop,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3) +
  labs(title = "10 Largest US Cities by Population") +
  ggthemes::theme_map()


#3.2 A map that colors USA cities by their distance from the national border
border_dist %>%
  slice_max(dist, n = 5) ->
  top5_to_bordermap

ggplot() +
  geom_sf(data = USboundaries) +
  geom_sf(data = border_dist, aes(col = dist), size = .1) +
  geom_sf(data = top5_to_bordermap, col = "red", size = 1) +
  scale_color_gradient(low = "grey", high = "blue") +
  ggrepel::geom_label_repel(
    data = top5_to_bordermap,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3) +
  labs(title = "US Cities' Distance from National Border") +
  ggthemes::theme_map()


#3.3 A map that colors USA cities by their distance from the nearest state border
border_dist2 %>%
  slice_max(dist2, n = 5) ->
  top5_to_statebordermap

ggplot() +
  geom_sf(data = USboundaries2) +
  geom_sf(data = border_dist2, aes(col = dist2), size = .1) +
  geom_sf(data = top5_to_statebordermap, col = "red", size = 1) +
  scale_color_gradient(low = "grey", high = "blue") +
  ggrepel::geom_label_repel(
    data = top5_to_statebordermap,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3) +
  labs(title = "US Cities' Distance from State Borders") +
  ggthemes::theme_map()

#3.4 Equidistance boundary from Mexico and Canada
uscities %>%
  select(city, state_name, population) %>%
  mutate(dist3 = st_distance(uscities, mexico_border),
         dist3 = units::set_units(dist3, "km"),
         dist3 = units::drop_units(dist3)) %>%
  mutate(dist4 = st_distance(uscities, canada_border),
         dist4 = units::set_units(dist4, "km"),
         dist4 = units::drop_units(dist4)) %>%
  mutate(diffdist = abs(dist3 - dist4)) ->
  equaldistance

equaldistance %>%
  filter(diffdist <= 100) %>%
  slice_max(population, n = 5) ->
  ED_pop

ggplot() +
  geom_sf(data = USboundaries2) +
  geom_sf(data = equaldistance, aes(col = diffdist), size = .1) +
  scale_color_gradient(low = "grey", high = "blue") +
  gghighlight::gghighlight(diffdist <= 100) +
  ggrepel::geom_label_repel(
    data = ED_pop,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3) +
  labs(title = "Cities that are equal distance from the Canadian and Mexican border ± 100 km") +
  ggthemes::theme_map()

#4.1 Quantifying Border Zone
uscities %>%
  select(city, state_name, population) %>%
  mutate(dist = st_distance(uscities, USboundaries),
         dist = units::set_units(dist, "km"),
         dist = units::drop_units(dist)) %>%
  filter(dist <= 160) %>%
  st_drop_geometry() ->
  bordercities

bordercities %>%
  summarize(population) %>%
  sum()

uscities %>%
  summarize(population) %>%
  sum()

data.frame(number_cities = "12283",
           pop = "259935815")





