# highway distance/price scatterplot
st_drop_geometry(testData) %>%
  dplyr::select(price, highwayDistance) %>%
  pivot_longer(cols = !price, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value, price)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", color = "#FA7800") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Price by distance to highway",
       subtitle = "Distance in miles") +
  plotTheme()

# highway distance map
ggplot() +
  geom_sf(data = countyLimits, fill = "gray98", color = "gray95", lwd = 0.5) +
  geom_sf(data = st_crop(coloradoHighways, countyLimits), color = "gray90", lwd = 3) +
  geom_sf(data = finalData, aes(color = highwayDistance)) +
  scale_color_viridis_c("Distance in meters", option = "E") +
  labs(title = "Distance from nearest highway",
       subtitle = "Individual homes relative to highway network",
       caption = "Figure X.X") +
  mapTheme()





# --- only major highways; didn't really work ---
boulderMajorHighways <- st_read('HighwaysByFunctionalClass.geojson') %>%
  dplyr::select(OBJECTID, FUNCCLASS, geometry) %>%
  st_transform(boulderCRS) %>%
  st_crop(.,st_buffer(countyLimits, 8045)) %>%
  filter(str_detect(FUNCCLASS, "Interstate") | str_detect(FUNCCLASS, "Principal")) %>%
  st_union(.)

highwayData <- homeIDs %>%
  mutate(highwayDistance = drop_units(st_distance(., boulderMajorHighways))) %>% # * 3.28 for ft
  st_drop_geometry()






boulderHighways <- st_buffer(countyLimits, 8045) %>%
  st_intersects(., coloradoHighways)
  
  
  
  st_intersects(coloradoHighways, st_buffer(countyLimits, 8045)) %>%
  st_sf()

unionHighways <- st_union(coloradoHighways) %>%
  st_sf()







ggplot() +
  geom_sf(data = countyLimits, fill = "gray50", color = "gray10", lwd = 0) +
  geom_sf(data = marijuanaData, aes(color = dispensaryDistance)) +
  geom_sf(data = marijuana, color = "orange", size = 0.2)

drop_units(highwayData$highwayDistance)



  
highwayData <- 



unionHighways <- st_union(coloradoHighways)


finalData$highwayDistance <- min(st_distance(finalData, coloradoHighways)) %>%
  drop_units()

distanceTest <- st_distance(finalData, coloradoHighways)

distanceTest2 <- min(st_distance(finalData, coloradoHighways))

finalData$unionDistance <- st_distance(finalData, unionHighways) %>%
  drop_units()



finalData <- finalData %>%
  mutate(
    logHighwayDistance = log(unionDistance)
  )
