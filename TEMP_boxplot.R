ggplot() +
  geom_boxplot(data = finalData,
               aes(x = builtEra,
                   y = price))