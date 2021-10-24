

coloradoHighways <- st_read('HighwaysByFunctionalClass.geojson') %>%
  st_transform(boulderCRS) 

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
