# flood risk/price scatterplot
st_drop_geometry(testData) %>%
  dplyr::select(price, floodRisk) %>%
  pivot_longer(cols = !price, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value, price)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", color = "#FA7800") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Price by flood risk level",
       subtitle = "Flood risk ") +
  plotTheme()

# opaque symbol map, dark background
ggplot() +
  geom_sf(data = countyLimits, fill = "gray50", color = "gray10", lwd = 0) +
  geom_sf(data = tracts, fill = "transparent", color = "gray15") +
  # geom_sf(data = floodplains, aes(fill = FLD_ZONE), alpha = 0.5, lwd=0) +
  geom_sf(data = floodrisk, aes(color = floodRisk), alpha = 1, size = 1) +
  scale_color_viridis_c("Flood zone", option = "G", breaks = c(0, 1, 2)) +
  labs(title = "Flood risk",
       subtitle = "Subtitle",
       caption = "Figure X.X") +
  mapTheme()


# opaque symbol map, dark background
ggplot() +
  geom_sf(data = countyLimits, fill = "gray50", color = "gray10", lwd = 0) +
  geom_sf(data = tracts, fill = "transparent", color = "gray15") +
  #geom_sf(data = floodplains, aes(fill = FLD_ZONE), alpha = 0.5, lwd=0) +
  geom_sf(data = floodData %>% filter(!is.na(floodRisk)), aes(color = floodRisk), alpha = 1, size = 1.5) +
  scale_color_viridis_b("Flood zone", option = "G", breaks = seq(0,4)) +
  labs(title = "Flood risk",
       subtitle = "Subtitle",
       caption = "Figure X.X") +
  mapTheme()