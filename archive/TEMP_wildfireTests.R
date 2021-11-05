# opaque symbol map, dark background
ggplot() +
  geom_sf(data = countyLimits, fill = "gray20", color = "gray10", lwd = 0) +
  geom_sf(data = tracts, fill = "transparent", color = "gray15") +
  # geom_sf(data = wildfires, color = "transparent", fill = "#c4a499", alpha = 0.4) +
  geom_sf(data = testData, aes(color = wildfireHistory), alpha = 0.7, size = 1.5) +
  # geom_sf(data = countyLimits, fill = "transparent", color = "black") +
  scale_color_viridis_c("Fires", option = "B", breaks = c(0, 2, 4, 6, 8)) +
  # scale_alpha_continuous(range = c(0.3, 0.7), guide = "none") +
  # scale_size_continuous(range = c(1, 3), breaks = c(0, 2, 4, 6, 8), guide = "none") +
  # guides("Wildfires", color = guide_legend(), size = guide_legend()) +
  labs(title = "Wildfire history",
       subtitle = "Wildfires within two miles of home in last 20 years",
       caption = "Figure X.X") +
  mapTheme()

# graduated symbol and alpha map, dark background
ggplot() +
  geom_sf(data = countyLimits, fill = "gray20", color = "gray10", lwd = 0) +
  geom_sf(data = wildfires, color = "transparent", fill = "#665550", alpha = 0.4) +
  geom_sf(data = testData,
          aes(color = wildfireHistory, alpha = wildfireHistory, size = wildfireHistory)) +
  # geom_sf(data = countyLimits, fill = "transparent", color = "black") +
  scale_color_viridis_c(option = "B", breaks = c(0, 2, 4, 6, 8)) +
  scale_alpha_continuous(range = c(0.3, 0.7), guide = "none") +
  scale_size_continuous(range = c(1, 3), breaks = c(0, 2, 4, 6, 8), guide = "none") +
  # guides("Wildfires", color = guide_legend(), size = guide_legend()) +
  labs(title = "Wildfire history",
       subtitle = "Wildfires within two miles of home in last 20 years",
       caption = "Figure X.X") +
  mapTheme()

# graduated symbol and alpha map, light background
ggplot() +
  geom_sf(data = countyLimits, fill = "gray95", color = "gray80", lwd = 0) +
  geom_sf(data = wildfires, color = "transparent", fill = "#c4a499", alpha = 0.4) +
  geom_sf(data = testData,
          aes(color = wildfireHistory, alpha = wildfireHistory, size = wildfireHistory)) +
  # geom_sf(data = countyLimits, fill = "transparent", color = "black") +
  scale_color_viridis_c(option = "B", breaks = c(0, 2, 4, 6, 8)) +
  scale_alpha_continuous(range = c(0.5, 0.8), guide = "none") +
  scale_size_continuous(range = c(1, 3), breaks = c(0, 2, 4, 6, 8), guide = "none") +
  # guides("Wildfires", color = guide_legend(), size = guide_legend()) +
  labs(title = "Wildfire history",
       subtitle = "Wildfires within two miles of home in last 20 years",
       caption = "Figure X.X") +
  mapTheme()

# graduated symbol map, dark background
ggplot() +
  geom_sf(data = countyLimits, fill = "gray20", color = "gray80", lwd = 0) +
  geom_sf(data = tracts, fill = "transparent", color = "gray15") +
  # geom_sf(data = wildfires, color = "transparent", fill = "#c4a499", alpha = 0.4) +
  geom_sf(data = testData,
          aes(color = wildfireHistory, size = wildfireHistory), alpha = 0.8) +
  # geom_sf(data = countyLimits, fill = "transparent", color = "black") +
  scale_color_viridis_c(option = "B", breaks = c(0, 2, 4, 6, 8)) +
  scale_size_continuous(range = c(1, 2), breaks = c(0, 2, 4, 6, 8)) +
  guides("Fires", color = guide_legend(), size = guide_legend()) +
  labs(title = "Wildfire history",
       subtitle = "Wildfires within two miles of home in last 20 years",
       caption = "Figure X.X") +
  mapTheme()

# graduated symbol map, light background
ggplot() +
  geom_sf(data = countyLimits, fill = "gray95", color = "gray80", lwd = 0) +
  # geom_sf(data = wildfires, color = "transparent", fill = "#c4a499", alpha = 0.4) +
  geom_sf(data = testData,
          aes(color = wildfireHistory, size = wildfireHistory), alpha = 0.8) +
  # geom_sf(data = countyLimits, fill = "transparent", color = "black") +
  scale_color_viridis_c(option = "F", breaks = c(0, 2, 4, 6, 8)) +
  scale_size_continuous(range = c(1, 3), breaks = c(0, 2, 4, 6, 8)) +
  guides("Wildfires", color = guide_legend(), size = guide_legend()) +
  labs(title = "Wildfire history",
       subtitle = "Wildfires within two miles of home in last 20 years",
       caption = "Figure X.X") +
  mapTheme()


# light fire color #c4a499
# darker fire color #665550

ggplot() +
  geom_sf(data = countyLimits, fill = "gray98", color = "black", lwd = 0.3) +
  geom_sf(data = wildfires, color = "transparent", fill = "#c4a499", alpha = 0.7) +
  geom_sf(data = testData,
          aes(color = wildfireHistory), alpha = 0.5) +
  # geom_sf(data = countyLimits, fill = "transparent", color = "black") +
  scale_color_viridis_c("Nearby fires",
                        option = "B") +
  labs(title = "Wildfire history",
       subtitle = "Wildfires within two miles of home in last 20 years",
       caption = "Figure X.X") +
  mapTheme()

testDataNoWildfires <- testData %>%
  filter(wildfireHistory == 1)

ggplot() +
  geom_sf(data = countyLimits) +
  geom_sf(data = testBuffer, color = "transparent", fill = "red", alpha = 0.01) +
  geom_sf(data = wildfires, color = "transparent", fill = "green", alpha = 0.5) +
  geom_sf(data = testDataNoWildfires,
          aes(color = wildfireHistory), alpha = 0.5) +
  scale_color_viridis_c() +
  labs(title = "Wildfire history") +
  mapTheme()


st_drop_geometry(testData) %>%
  dplyr::select(price, wildfireHistory) %>%
  pivot_longer(cols = !price, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value, price)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", color = "#FA7800") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Price by wildfire history",
       subtitle = "Wildfires within two miles of home in last 20 years") +
  plotTheme()

# --- WILDFIRES WITHIN 2 MILES IN LAST 20 YEARS: actually helped! ---
# ... except the calculation is wrong, fuck

wildfires <-
  st_read('Wildfire_History.geojson') %>%
  filter(ENDDATE > "2001-10-19 00:00:00") %>% # FILTER to only fires that happened after 2000
  select(NAME, geometry) %>%
  st_transform(boulderCRS)

wildfireData <- homeIDs

wildfireData$wildfireHistory <- st_buffer(homeIDs, 3219) %>%
  aggregate(mutate(wildfires, counter = as.numeric(1)), ., length) %>%
  pull(counter) %>%
  replace_na(., 0)

wildfireData <- wildfireData %>%
  st_drop_geometry()


# --- WILDFIRES IN LAST 20 YEARS: slightly worsened model ---
wildfires <-
  st_read('Wildfire_History.geojson') %>%
  filter(ENDDATE > "2001-10-19 00:00:00") %>% # FILTER to only fires that happened after 2000
  select(NAME, geometry) %>%
  st_transform(boulderCRS)

wildfireData <- homeIDs %>%
  mutate(
    wildfireHistory = lengths(st_intersects(homeIDs, wildfires))
  ) %>%
  st_drop_geometry()

# --- ORIGINAL WILDFIRE BUFFER: no real difference to model ---

wildfires <-
  st_read('Wildfire_History.geojson') %>%
  filter(ENDDATE > "2001-10-19 00:00:00") %>% # FILTER to only fires that happened after 2000
  select(NAME, geometry) %>%
  st_transform(boulderCRS) %>%
  st_buffer(1610) %>%
  st_union() %>%
  st_sf() %>%
  mutate(wildfireHistory = 1)

wildfireData <- st_join(homeIDs, wildfires) %>%
  st_drop_geometry() %>%
  mutate(wildfireHistory = replace_na(wildfireHistory, 0))