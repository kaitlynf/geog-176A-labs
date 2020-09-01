library(tidyverse)

library(ggplot2)

library(sf)

library(rmapshaper)

library(readxl)

dplyr::id()

#step 1
USAboundaries::us_counties() %>%
  filter(!state_name %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
  st_transform(5070) ->
  counties

st_centroid(counties) %>%
  st_union() ->
  cent_counties

#voroni tessellation
st_voronoi(cent_counties) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n()) ->
  v_grid

st_intersection(v_grid, st_union(counties)) ->
  v_grid

rmapshaper::ms_simplify(v_grid, keep = .01) %>%
  plot()

#triangulated tessellation
st_triangulate(cent_counties) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n()) ->
  t_grid

plot(t_grid)
#grid coverage
st_make_grid(counties, n = c(70, 50)) %>%
  st_as_sf() %>%
  mutate(id = 1:n()) ->
  sq_grid

#hex coverage
st_make_grid(counties, n = c(70, 50), square = FALSE) %>%
  st_as_sf() %>%
  mutate(id = 1:n()) ->
  hex_grid

#ggplot function
plot_tess = function(data, title){
  ggplot() +
    geom_sf(data = data, fill = "white", col = "navy", size = .2) +
    theme_void() +
    labs(title = title, caption = paste("This tesselation has:", nrow(data), "tiles" ))
}

plot_tess(hex_grid, "hexagonal grid coverage")

plot_tess(sq_grid, "square grid coverage")

plot_tess(t_grid, "triangulated tessellation")

plot_tess(v_grid, "voroni tessellation")

plot_tess(counties, "counties")

#QUESTION 2

tess_features = function(data, title){
  df = data.frame(row.names = title,
    mean_area = units::set_units((st_area(data)), "km^2") %>%
                    units::drop_units() %>%
                    head(n = 1),
                  total_area = units::set_units((sum(st_area(data))), "km^2") %>%
                    units::drop_units() %>%
                    head(n = 1),
                  n_features = count(hex_grid, "id") %>%
                    st_drop_geometry() %>%
                    select(n),
                  standard_dev = sd(st_area(data)))
  return(df)
}

sd(st_area(hex_grid))

sd(c(hex_grid), na.rm = FALSE)

tess_features(hex_grid, "hex grid summary")

tess_summary = bind_rows(
  tess_features(counties, "original counties summary"),
  tess_features(sq_grid, "square grid summary"),
  tess_features(hex_grid, "hexagonal grid summary"),
  tess_features(v_grid_simp, "voroni tessellation summary"),
  tess_features(t_grid_simp, "triangulated tessellation summary"))

knitr::kable(tess_summary,
             caption = "Tessellation Summary",
             col.names = c("Mean Area", "Total Area", "Number of Features"))

#Question 3

read_excel("data/NID2019_U.xlsx") %>%
  filter(!is.na(LATITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(5070) %>%
  st_filter(counties) ->
  NID

#hex grid example
st_join(hex_grid, NID) %>%
  st_cast("MULTIPOLYGON") %>%
  st_drop_geometry() %>%
  count(COUNTY) %>%
  setNames(c("COUNTY", "n")) %>%
  left_join(hex_grid, by = COUNTY)

point_in_polygon = function(points, polygon, group){
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    count(get(group)) %>%
    setNames(c(group, "n")) %>%
    left_join(polygon, by = group) %>%
    st_as_sf()
}

point_in_polygon(NID, hex_grid, "id") %>%
  plot_pip()

plot_pip = function(data, title){
  ggplot() +
    geom_sf(data = data, aes(fill = log(n)), col = NA) +
    scale_fill_viridis_b() +
    theme_void() +
    labs(title = "Dam Locations",
         caption = paste("Dams:", sum(data$n)))
}

unique(NID$PURPOSES) %>%
  length

NID %>%
 filter(grepl("S", PURPOSES)) ->
  water_supply

NID %>%
  filter(grepl("R", PURPOSES)) ->
  recreation

point_in_polygon(recreation, sq_grid, "id") %>%
  plot_pip()
