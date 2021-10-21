library(tidyverse)
library(tidycensus)
library(sf)

boulderCRS <- 'ESRI:102253' # NAD 1983 HARN StatePlane Colorado North FIPS 0501

data <- st_read("studentData.geojson") %>%
  st_set_crs('ESRI:102254') %>%
  st_transform(boulderCRS)

eraRecodeTemp <- data %>%
  mutate(
    # calculate log of price to normalize positive skew
    logPrice = log(price),
    # recode missing construction material values
    constMat = case_when(
      ConstCode == 0 ~ "Missing",
      ConstCode == 300 ~ "Unspecified",
      ConstCode > 300 ~ as.character(ConstCodeDscr)
    ),
    # recode missing basement values
    basement = if_else(bsmtType == 0, "None", as.character(bsmtTypeDscr)),
    # recode missing car storage values
    carStorage = if_else(carStorageType == 0, "None", as.character(carStorageTypeDscr)),
    # recode missing a/c values
    acType = case_when(
      is.na(Ac) ~ "None",
      Ac == 200 ~ "Unspecified", # Code with no description
      Ac >= 210 ~ as.character(AcDscr) # "Attic Fan", "Evaporative Cooler", "Whole House" unchanged
    ),
    # recode missing heating values
    heatingType = case_when(
      is.na(Heating) ~ "None",
      Heating == 800 ~ "Unspecified",
      Heating > 800 ~ as.character(HeatingDscr)
    ),
    # recode missing primary exterior wall values
    extWall = if_else(ExtWallPrim == 0, "Missing", as.character(ExtWallDscrPrim)),
    # recode missing secondary exterior wall values
    extWall2 = if_else(is.na(ExtWallSec), "None", as.character(ExtWallDscrSec)),
    # recode missing interior wall values
    intWall = if_else(is.na(IntWall), "Missing", as.character(IntWallDscr)),
    # recode missing roof cover values
    roofType = if_else(is.na(Roof_Cover), "Missing", as.character(Roof_CoverDscr)),
    # recode quality as numeric variable
    # NOTE: not at all normal; maybe note in writeup if using
    qualityNum = case_when(
      qualityCode == 10 ~ 1, # QualityCodeDscr == "LOW "
      qualityCode == 20 ~ 2, # "FAIR "
      qualityCode == 30 ~ 3, # "AVERAGE "
      qualityCode == 31 ~ 4, # "AVERAGE + "
      qualityCode == 32 ~ 5, # "AVERAGE ++ "
      qualityCode == 40 ~ 6, # "GOOD "
      qualityCode == 41 ~ 7, # "GOOD + "
      qualityCode == 42 ~ 8, # "GOOD ++ "
      qualityCode == 50 ~ 9, # "VERY GOOD "
      qualityCode == 51 ~ 10, # "VERY GOOD + "
      qualityCode == 52 ~ 11, # "VERY GOOD ++ "
      qualityCode == 60 ~ 12, # "EXCELLENT "
      qualityCode == 61 ~ 13, # "EXCELLENT + "
      qualityCode == 62 ~ 14, # "EXCELLENT++ "
      qualityCode == 70 ~ 15, # "EXCEPTIONAL 1 "
      qualityCode == 80 ~ 16, # "EXCEPTIONAL 2 "
    ),
    # recode builtYear as builtEra
    builtEra = case_when(
      builtYear < 1910 ~ "Pre-1910",
      between(builtYear, 1910, 1919) ~ "1910s",
      between(builtYear, 1920, 1929) ~ "1920s",
      between(builtYear, 1930, 1939) ~ "1930s",
      between(builtYear, 1940, 1949) ~ "1940s",
      between(builtYear, 1950, 1959) ~ "1950s",
      between(builtYear, 1960, 1969) ~ "1960s",
      between(builtYear, 1970, 1979) ~ "1970s",
      between(builtYear, 1980, 1989) ~ "1980s",
      between(builtYear, 1990, 1999) ~ "1990s",
      between(builtYear, 2000, 2009) ~ "2000s",
      between(builtYear, 2010, 2019) ~ "2010s",
      builtYear >= 2020 ~ "2020s",
    )
  )

eraRecode <- eraRecodeTemp %>%
  # exclude extreme outlier listed as sold for $31.5 million; 
  # listing strongly suggests incorrectly entered: https://www.zillow.com/homedetails/3335-Talisman-Ct-APT-C-Boulder-CO-80301/13222117_zpid/
  filter(MUSA_ID != 8735) %>%
  # drop unneeded columns
  dplyr::select(
    # same for all
    -bldgClass,
    -bldgClassDscr,
    -status_cd,
    # not needed
    -saleDate,
    -address,
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
    -qualityCodeDscr,
    -builtYear
  )

# get census tracts
census_api_key("e79f3706b6d61249968c6ce88794f6f556e5bf3d", overwrite = FALSE)

year <- 2019
state <- 08
county <- 13

tracts <- 
  get_acs(geography = "tract",
          variables = "B01001_001",
          year = year,
          state = state,
          county = county,
          geometry = TRUE,
          output = 'wide') %>%
  rename(totalPop = B01001_001E) %>% # Total population
  dplyr::select(GEOID, geometry) %>%
  st_transform(st_crs(boulderCRS))

eraWithTracts <- st_join(eraRecode, tracts) %>%
  rename(tractID = GEOID)

countyZoning <- st_read('Zoning_-_Zoning_Districts.geojson') %>%
  st_transform(st_crs(boulderCRS)) %>%
  dplyr::select(ZONECLASS, ZONEDESC, geometry) %>%
  mutate(
    city = if_else(str_detect(ZONECLASS, "X"), as.character(ZONEDESC), "Unincorporated"),
  )

eraTractCity <- st_join(eraWithTracts, countyZoning) %>%
  dplyr::select(-ZONECLASS, -ZONEDESC)

subcommunities <- st_read('Subcommunities.geojson') %>%
  st_transform(st_crs(boulderCRS)) %>%
  dplyr::select(SUBCOMMUNITY, geometry)

zillowHoods <- st_read('ZillowNeighborhoodsBoulderCounty.geojson') %>%
  st_transform(st_crs(boulderCRS)) %>%
  dplyr::select(Name, geometry)

eraTractCitySubTemp <- st_join(eraTractCity, subcommunities) %>%
  mutate(
    subcommunity = if_else(is.na(SUBCOMMUNITY), "None", as.character(SUBCOMMUNITY))
  )

etCitySubWithLongmontTemp <- st_join(eraTractCitySubTemp, zillowHoods) %>%
  mutate(
    zillowHood = if_else(is.na(Name), "None", as.character(Name)),
    subcommunity = if_else(city == "Longmont", as.character(zillowHood), as.character(subcommunity))
  )

etCitySubWithLongmont <- etCitySubWithLongmontTemp %>%
  dplyr::select(-SUBCOMMUNITY, -Name, -zillowHood)

# new data frame
bestSoFar <- etCitySubWithLongmont

# calculate spatial lag
coords <- st_coordinates(etCitySubWithLongmont)

neighborList <- knn2nb(knearneigh(coords, 5))

spatialWeights <- nb2listw(neighborList, style = "W")

bestSoFar$logLagPrice <- log(lag.listw(spatialWeights, bestSoFar$price, NAOK = TRUE))

# REMOVE COLINEAR VARIABLES
bestSoFar_noBldNum <- 
  dplyr::select(bestSoFar, -bld_num) %>%
  mutate(manySections = if_else(section_num > 1, 1, 0))

bestSoFar_SecNumRecode <- 
  dplyr::select(bestSoFar_noBldNum2, -section_num)

bestSoFar_noSecNum <- 
  dplyr::select(bestSoFar, -section_num)


bestSoFar_recodes <- bestSoFar %>%
  mutate(
    manySections = if_else(section_num > 1, 1, 0)
  )

secNumRecode <- bestSoFar_recodes %>%
  dplyr::select(
    -bld_num,
    -section_num,
  )
  
# --- MODEL ESTIMATION & VALIDATION ---

regData <- secNumRecode # UPDATE WHEN RUNNING NEW MODEL

homes.training <- filter(regData, toPredict == 0)
homes.test <- filter(regData, toPredict == 1)

# TODO: Estimate model on training set
reg.training <- lm(logPrice ~ .,
                   data = st_drop_geometry(regData) %>%
                     dplyr::select(-toPredict, -MUSA_ID, -price)
)

summary(reg.training)

homes.test <- homes.test %>%
  mutate(
    logPrice.Predict = predict(reg.training, homes.test),
    price = exp(logPrice.Predict)
  )

submission <- homes.test %>%
  dplyr::select(MUSA_ID, price) %>%
  st_drop_geometry()

write.csv(submission,"89ers.csv", row.names = TRUE)

