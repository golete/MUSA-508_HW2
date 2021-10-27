# --- TEST OPEN DATA ---

wholeFoodsRaw <- st_read("wholefoodsmarkets_boulderCO.csv")

wholeFoods <- st_as_sf(wholeFoodsRaw, coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::select(ID, geometry) %>%
  st_sf() %>%
  st_transform(boulderCRS)

wholeFoodsData <- homeIDs %>%
  mutate(wholeFoodsDistance = nn_function(st_coordinates(.), st_coordinates(wholeFoods), 1)) %>%
  st_drop_geometry()


coloradoHighways <- st_read('HighwaysByFunctionalClass.geojson') %>%
  dplyr::select(OBJECTID, FUNCCLASS, geometry) %>%
  st_transform(boulderCRS) %>%
  st_crop(.,st_buffer(countyLimits, 8045)) %>% # 3218
  st_union(.)

highwayData <- homeIDs %>%
  mutate(highwayDistance = log(drop_units(st_distance(., coloradoHighways)))) %>% # * 3.28 for ft
  st_drop_geometry()

# --- dispensaries ---
dispensaries <- st_read("Marijuana_Establishments.geojson") %>%
  filter(str_detect(Description, "Store")) %>%
  dplyr::select(OBJECTID, Type, geometry) %>%
  st_sf() %>%
  st_transform(boulderCRS)

dispensaryData <- homeIDs %>%
  mutate(dispensaryDistance = nn_function(st_coordinates(.), st_coordinates(marijuana), 1) / 1609) %>%
  st_drop_geometry()
  
# --- A. HOME VALUE DATA ---

testRecodes <- homeRecodes %>%
  mutate(
    # extWall = case_when(
    #   is.na(ExtWallPrim) | ExtWallPrim == 0 ~ "Missing",
    #   ExtWallPrim %in% c(20, 25, 90, 130) ~ "Other",
    #   TRUE ~ as.character(ExtWallDscrPrim)
    # )
  )

# create clean data frame for modeling
homeData <- testRecodes %>%
  # drop extreme outliers identified as data entry errors
  filter(!MUSA_ID %in% c(8735,1397,5258)) %>%
  # drop unneeded columns
  dplyr::select(
    # TESTING
    # same for all
    -bldgClass,
    -bldgClassDscr,
    -status_cd,
    # not needed
    -saleDate,
    -address,
    -bld_num,
    # redundant
    -year,
    # too much missing data
    -Stories,
    -UnitCount,
    # cleaned
    -designCode,
    -qualityCode,
    -ConstCode,
    -ConstCodeDscr,
    -bsmtType,
    -bsmtTypeDscr,
    -carStorageType,
    -carStorageTypeDscr,
    -Ac,
    -AcDscr,
    -Heating,
    -HeatingDscr,
    -ExtWallPrim,
    -ExtWallDscrPrim,
    -ExtWallSec,
    -ExtWallDscrSec,
    -IntWall,
    -IntWallDscr,
    -Roof_Cover,
    -Roof_CoverDscr,
    # recoded
    -section_num,
    -qualityCodeDscr,
    -builtYear,
    -designCodeDscr
  )

# # isolate home IDs to use in spatial joins
# homeIDs <- data %>%
#   dplyr::select(MUSA_ID, geometry)

# --- B. NEIGHBORHOOD BOUNDARIES ---

# B1. Census tracts

# # set Census data import parameters
# year <- 2019
# state <- 08
# county <- 13
# 
# # import Census tract boundaries as proxy for neighborhoods
# # TODO: also import median income here for generalizability test later on?
# tracts <- 
#   get_acs(geography = "tract",
#           variables = "B19013_001E", # median household income
#           year = year,
#           state = state,
#           county = county,
#           geometry = T,
#           output = "wide") %>%
#   dplyr::select(GEOID, B19013_001E, geometry)%>%
#   rename(tract = GEOID,
#          medianIncome = B19013_001E) %>%
#   st_transform(boulderCRS)
# 
# # isolate tract boundaries to join to home data
# tractsData <- st_join(homeIDs, tracts) %>%
#   dplyr::select(-medianIncome) %>%
#   st_drop_geometry()

# --- C. AMERICAN COMMUNITY SURVEY DATA ---

# # review available variables
# acsVariableList <- load_variables(year,"acs5", cache = TRUE)
# 
# # define variables to import
# acsVars <- c("B02001_001E", # race: total
#              "B02001_002E", # race: white alone
#              'B25003_001E', # tenure: occupied housing units
#              'B25003_002E', # tenure: owner-occupied
#              'B25002_001E', # occupancy: total housing units
#              'B25002_003E', # occupancy: vacant housing units
#              'B15003_001E', # educational attainment: total
#              'B15003_022E', # educational attainment: bachelor's degree
#              'B15003_023E', # educational attainment: master's degree
#              'B15003_024E', # educational attainment: professional degree
#              'B15003_025E', # educational attainment: doctorate degree
#              'B19001_001E', # household income: total
#              'B19001_002E', # household income: less than $10k
#              'B19001_003E', # household income: $10-15k
#              'B19001_004E', # household income: $15-20k
#              'B19001_005E', # household income: $20-25k
#              'B19001_006E', # household income: $25-30k
#              'B19001_007E', # household income: $30-35k
#              'B19001_008E', # household income: $35-40k
#              'B19001_009E', # household income: $40-45k
#              'B19001_010E', # household income: $45-50k
#              'B19001_011E', # household income: $50-60k
#              'B19001_012E', # household income: $60-75k
#              'B19001_013E', # household income: $75-100k
#              'B19001_014E', # household income: $100-125k
#              'B19001_015E', # household income: $125-150k
#              'B19001_016E', # household income: $150-200k
#              'B19001_017E') # household income: $200 or more
# 
# # import variables from ACS 2019 5-year
# blockGroups <- 
#   get_acs(geography = "block group",
#           variables = acsVars,
#           year = year,
#           state = state,
#           county = county,
#           geometry = T,
#           output = 'wide') %>%
#   dplyr::select(-ends_with('M')) %>%
#   rename(# white population
#     raceTotal = B02001_001E, # race: total
#     whiteAlone = B02001_002E, # race: white alone
#     # vacant housing units
#     totalUnits = B25002_001E, # occupancy status: total
#     vacantUnits = B25002_003E, # occupancy status: vacant
#     # homeowners
#     occupiedUnits = B25003_001E, # tenure: total
#     ownerOccupied = B25003_002E, # tenure: owner-occupied
#     # highest educational attainment
#     eduTotal = B15003_001E, # educational attainment: total
#     eduBachs = B15003_022E, # educational attainment: bachelor's degree
#     eduMasts = B15003_023E, # educational attainment: master's degree
#     eduProfs = B15003_024E, # educational attainment: professional degree
#     eduDocts = B15003_025E, # educational attainment: doctorate degree
#     # household income
#     incomeTotal = B19001_001E, # household income: total
#     income000 = B19001_002E, # household income: less than $10k
#     income010 = B19001_003E, # household income: $10-15k
#     income015 = B19001_004E, # household income: $15-20k
#     income020 = B19001_005E, # household income: $20-25k
#     income025 = B19001_006E, # household income: $25-30k
#     income030 = B19001_007E, # household income: $30-35k
#     income035 = B19001_008E, # household income: $35-40k
#     income040 = B19001_009E, # household income: $40-45k
#     income045 = B19001_010E, # household income: $45-50k
#     income050 = B19001_011E, # household income: $50-60k
#     income060 = B19001_012E, # household income: $60-75k
#     income075 = B19001_013E, # household income: $75-100k
#     income100 = B19001_014E, # household income: $100-125k
#     income125 = B19001_015E, # household income: $125-150k
#     income150 = B19001_016E, # household income: $150-200k
#     income200 = B19001_017E # household income: $200k or more
#   )%>%
#   mutate(pctWhite = whiteAlone/raceTotal,
#          pctVacant = vacantUnits/totalUnits,
#          pctOwnerOccupied = ownerOccupied/occupiedUnits,
#          # calculate percent with bachelor's or higher
#          # TODO: compare percent postgraduate?
#          pctHigherEdu = if_else(
#            eduTotal > 0, (eduBachs + eduMasts + eduProfs + eduDocts)/eduTotal, 0
#          ),
#          # calculate percent in each income category
#          pctIncome000 = income000/incomeTotal,
#          pctIncome010 = income010/incomeTotal,
#          pctIncome015 = income015/incomeTotal,
#          pctIncome020 = income020/incomeTotal,
#          pctIncome025 = income025/incomeTotal,
#          pctIncome030 = income030/incomeTotal,
#          pctIncome035 = income035/incomeTotal,
#          pctIncome040 = income040/incomeTotal,
#          pctIncome045 = income045/incomeTotal,
#          pctIncome050 = income050/incomeTotal,
#          pctIncome060 = income060/incomeTotal,
#          pctIncome075 = income075/incomeTotal,
#          pctIncome100 = income100/incomeTotal,
#          pctIncome125 = income125/incomeTotal,
#          pctIncome150 = income150/incomeTotal,
#          pctIncome200 = income200/incomeTotal,
#          # recode final income features after exploratory analysis 
#          pctIncomeBelow100k = (
#            income000 + income010 + income015 + income020 + income025 + 
#              income030 + income035 + income040 + income045 + income050 + 
#              income060 + income075
#          )/incomeTotal,
#          pctIncomeAbove200k = pctIncome200
#   ) %>%
#   select(GEOID, pctWhite, pctVacant, pctOwnerOccupied, pctHigherEdu, 
#          pctIncomeBelow100k, pctIncomeAbove200k, geometry) %>%
#   rename(blockGroup = GEOID) %>%
#   st_transform(boulderCRS)
# 
# blockGroupBoundaries <- blockGroups %>%
#   select(blockGroup, geometry)
# 
# censusData <- st_join(homeIDs, blockGroupBoundaries) %>%
#   st_drop_geometry() %>%
#   left_join(., blockGroups, by="blockGroup") %>%
#   dplyr::select(-blockGroup, -geometry)

# D. --- ADDITIONAL OPEN DATA ---

# TODO: add open data

# D1. wildfire history
# TODO: add directly to finalData without merging?

# load wildfire polygon data, limited to fires in last 20 years
wildfires <-
  st_read('Wildfire_History.geojson') %>%
  filter(ENDDATE > "2001-10-19 00:00:00") %>%
  select(NAME, geometry) %>%
  st_transform(boulderCRS)

# get home locations
wildfireData <- homeIDs

# count wildfires within two-mile radius
wildfireData$wildfireHistory <- st_buffer(homeIDs, 3219) %>% # 3219 m = 2 miles
  aggregate(mutate(wildfires, counter = as.numeric(1)), ., length) %>%
  pull(counter) %>%
  replace_na(., 0)

# prepare for joining to main data set
wildfireData <- wildfireData %>%
  st_drop_geometry()

# D2. Flood risk

# floodplains <- 
#   st_read('Floodplain_-_BC_Regulated.geojson') %>%
#   st_transform(boulderCRS) %>%
#   dplyr::select(FLD_ZONE, geometry)

floodRecode <- st_join(homeIDs, floodplains) %>%
  mutate(
    floodRisk = case_when(
      is.na(FLD_ZONE) ~ 0,
      FLD_ZONE == "X" ~ 1,
      str_detect(FLD_ZONE, "A") ~ 2
    )
  )

floodData <- floodRecode %>%
  dplyr::select(-FLD_ZONE) %>%
  st_drop_geometry()

# E. --- COMBINE ALL ---

testData <- left_join(homeData, tractsData) %>%
  left_join(., censusData) %>%
  left_join(., wildfireData) %>%
  left_join(., floodData) %>%
  left_join(., marijuanaData) %>%
  left_join(., wholeFoodsData)