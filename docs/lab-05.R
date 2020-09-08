getlandsat::lsat_scenes() ->
  scenes

AOI %>%
  st_transform(4326) %>%
  st_bbox() ->
  AOIwgs

filtered_scenes = scenes %>%
  filter(min_lat <= AOIwgs$ymin, max_lat >= AOIwgs$ymax,
         min_lon <= AOIwgs$xmin, max_lon >= AOIwgs$xmax,
         as.Date(acquisitionDate) == as.Date("2016-09-26"))

write.csv(filtered_scenes, file = "data/palo-flood-scene.csv", row.names = FALSE)
