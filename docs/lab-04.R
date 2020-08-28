library(tidyverse)

library(ggplot2)

library(sf)

library(rmapshaper)

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
