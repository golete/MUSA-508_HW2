# import variables from ACS 2019 5-year
unifiedSchoolDistricts <- 
  get_acs(geography = "school district (unified)",
          variables = "B01001_001",
          year = year,
          state = state,
          geometry = T,
          output = 'wide') %>%
  rename(totalPop = B01001_001E) %>% # Total population
  st_transform(st_crs(data)) 

ggplot() +
  geom_sf(data = unifiedSchoolDistricts)