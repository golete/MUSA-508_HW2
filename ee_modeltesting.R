library(tidyverse)
library(tidycensus)
library(sf)

boulderCRS <- 'ESRI:102253' # NAD 1983 HARN StatePlane Colorado North FIPS 0501

# a. home value data
data <- st_read("studentData.geojson") %>%
  st_set_crs(boulderCRS)

ggplot() +
  geom_sf(data = data)