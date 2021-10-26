# calculate vehicle age; drop unneeded columns
builtEra.data <- finalData %>%
  dplyr::select(MUSA_ID, price, builtEra)

# calculate mean and median by income group
builtEra.summary <- builtEra.data %>%
  group_by(builtEra) %>%
  summarize(meanPrice = mean(price),
            medianPrice = median(price))

# plot mean and median vehicle age by income on same chart
ggplot() +
  geom_bar(data = builtEra.summary,
           aes(x = builtEra, y = medianPrice, fill = "Median"),
           stat = "identity") +
  scale_fill_manual(name = "", values = c("Median" = "#00bfc4")) +
  geom_bar(data = builtEra.summary,
           aes(x = builtEra, y = meanPrice, color = "Mean"),
           stat = "identity",
           fill = "transparent") +
  scale_color_manual(name = "", values = c("Mean" = "black")) +
  scale_x_discrete(name = "Era built") +
  scale_y_continuous(name = "Sale price") +
  labs(title = "Home price by era built",
       subtitle = "Mean and median",
       caption = "Figure X.X") +
  theme(legend.position = "bottom") +
  plotTheme()