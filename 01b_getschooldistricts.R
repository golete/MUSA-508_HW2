# set global CRS
boulderCRS <- 'ESRI:102253' # NAD 1983 HARN StatePlane Colorado North FIPS 0501

ggplot() +
  geom_sf(data = unifiedSchoolDistrictsCO) +
  geom_sf(data = countylimits, color = "red") +
  geom_sf(data = countyLimits, color = "blue")

# load county limits
countyLimits <- st_read('County_Boundary.geojson') %>%
  select(OBJECTID, geometry) %>%
  st_transform(boulderCRS)

# load Colorado school district boundaries
coloradoSchoolDistricts <- st_read("UnifiedSchoolDistrictsCO.geojson") %>%
  select(GEOID, NAME) %>%
  st_transform(boulderCRS)

addSchoolDistricts <- st_join()




