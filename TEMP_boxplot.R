ggplot() +
  geom_boxplot(data = finalData,
               aes(x = roofType,
                   y = price))

table(finalData$roofType)

ggplot() +
  geom_boxplot(data = testData,
               aes(x = roofType,
                   y = price))

table(testData$roofType)
