# --- Residuals ---
# map residuals for test set
ggplot() +
  geom_sf(data = countyLimits, fill = "gray99") +
  geom_sf(
    data = homes.test %>%
      arrange(price.AbsError),
    size = 2,
    aes(color = price.Error, 
        alpha = price.AbsError),
  ) +
  geom_sf(data = munis, fill = "transparent", size = 0.5) +
  scale_color_gradient("Prediction error",
                       low = "#fa7800",
                       high = "#25cb10",
                       label = scales::dollar_format()) +
  scale_alpha_continuous(range = c(0.2, 1),
                         guide = "none") +
  geom_sf(data = countyLimits, fill = "transparent") +
  labs(
    title = "Spatial distribution of prediction errors",
    subtitle = "Individual homes relative to county and municipal boundaries",
    caption = "Fig. X.X" # TODO: number figure before finalizing
  ) +
  mapTheme()
