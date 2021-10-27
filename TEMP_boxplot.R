ggplot() +
  geom_boxplot(data = finalData,
               aes(x = extWall,
                   y = price))

table(finalData$extWall)

ggplot() +
  geom_boxplot(data = testData,
               aes(x = extWall,
                   y = price))

table(testData$extWall)
