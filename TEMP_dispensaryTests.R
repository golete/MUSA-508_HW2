# dispensary/price scatterplot
st_drop_geometry(testData) %>%
  dplyr::select(price, dispensaryDistance) %>%
  pivot_longer(cols = !price, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value, price)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", color = "#FA7800") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Price by distance to cannabis dispensary",
       subtitle = "Distance in miles") +
  plotTheme()


# log dispensary/price scatterplot
st_drop_geometry(finalData) %>%
  dplyr::select(price, log(DispensaryDistance)) %>%
  pivot_longer(cols = !price, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value, price)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", color = "#FA7800") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Price by distance to cannabis dispensary",
       subtitle = "Distance in miles") +
  plotTheme()


ggplot() +
  geom_sf(data = countyLimits, fill = "gray98", color = "gray85", lwd = 0.5) +
  geom_sf(data = tracts, fill = "transparent", color = "gray90") +
  geom_sf(data = finalData, aes(color = dispensaryDistance)) +
  scale_color_viridis_c("Distance in meters", option = "D", direction = -1) +
  geom_sf(data = dispensaries, aes(fill = Type), size = 3, shape = 25) +
  scale_fill_manual("", labels = "Dispensary", values = "black") +
  labs(title = "Distance to recreational cannabis dispensary",
       subtitle = "Individual homes with Census tracts and dispensary locations",
       caption = "Figure X.X") + # TODO: number before finalizing
  mapTheme()
  
  
  geom_sf(data = tracts, fill = "transparent", color = "gray15") +
  # geom_sf(data = floodplains, aes(fill = FLD_ZONE), alpha = 0.5, lwd=0) +
  geom_sf(data = floodrisk, aes(color = floodRisk), alpha = 1, size = 1) +
  scale_color_viridis_c("Flood zone", option = "G", breaks = c(0, 1, 2)) +
  labs(title = "Flood risk",
       subtitle = "Subtitle",
       caption = "Figure X.X") +
  mapTheme()