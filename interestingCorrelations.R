# load libraries
library(tidyverse)
library(tidycensus)
library(sf)
library(dummies)
library(spdep)
library(caret)
# library(ckanr) # TODO: Remove if not needed; used to read CKAN APIs
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(kableExtra)
library(jtools)     # for regression model plots
library(ggstance) # to support jtools plots

# avoid scientific notation
options(scipen = 999)

boulderCRS <- 'ESRI:102253' # NAD 1983 HARN StatePlane Colorado North FIPS 0501

data <- st_read("studentData.geojson") %>%
  st_set_crs('ESRI:102254') %>%
  st_transform(boulderCRS) %>%
  filter(toPredict == 0) %>%
  # exclude extreme outliers identified as data entry errors;
  # MUSA_ID 8735 is listed at $31.5 million but sold for $315,000: 
  # https://www.zillow.com/homedetails/3335-Talisman-Ct-APT-C-Boulder-CO-80301/13222117_zpid/
  # 1397 is listed at $5 million but sold for $500,000:
  # https://www.zillow.com/homedetails/712-Sedge-Way-Lafayette-CO-80026/13232125_zpid/
  filter(!MUSA_ID %in% c(8735, 1397))

homeRecodes <- data %>%
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
      builtYear >= 2020 ~ "2020s"
    ),
    # recode section_num as manySections
    manySections = if_else(section_num > 1, 1, 0)
  )

homeData <- homeRecodes %>%
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
    -builtYear,
    -section_num,
    # colinear and/or not predictive
    -bld_num
  )

# ADD CENSUS TRACTS
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

tractData <- st_join(homeData, tracts) %>%
  rename(tractID = GEOID)

# new data frame
finalData <- tractData


# --- 4 INTERESTING CORRELATION SCATTERPLOTS ---
# Present 4 home price correlation scatterplots that you think are of interest. 
# I’m going to look for interesting open data that you’ve integrated with the 
# home sale observations

# from 'dataset' aka ACS variables:
# PCT25yrHighEdu most correlated, then PCTHHwhite (but fairly weak), then 
# PCTHHowner, then PCTVacant


# plot correlation of individual variables with home values - Census pt 1
st_drop_geometry(dataset) %>% 
  dplyr::select(price, PCTHHowner, PCTVacant, PCTHHwhite, PCT25yrHighEdu) %>% # REPLACE WITH VARIABLES TO CHECK
  pivot_longer(cols = !price, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value, price)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", color = "#FA7800") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Price as a function of continuous variables")

# plot correlation of individual variables with home values - Census pt 2
st_drop_geometry(dataset) %>% 
  dplyr::select(price, PCTbel125pov, PCT125185pov, PCT185500pov, PCTabo500pov) %>% # REPLACE WITH VARIABLES TO CHECK
  pivot_longer(cols = !price, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value, price)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", color = "#FA7800") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Price as a function of continuous variables")


# TODO: Try with different variables
st_drop_geometry(dataset) %>% 
  filter(!MUSA_ID %in% c(8735, 1397)) %>%
  dplyr::select(price, age, effectiveAge) %>% # REPLACE WITH VARIABLES TO CHECK
  pivot_longer(cols = !price, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value, price)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", color = "#FA7800") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Price as a function of continuous variables")

ggplot(dataset, aes(x = age, y = price))+
  geom_line(stat = "summary", fun = "median") +
  geom_smooth(method = "lm", color = "#FA7800") +
  labs(title = "median price by age")

ggplot(dataset, aes(x = effectiveAge, y = price))+
  geom_line(stat = "summary", fun = "median") +
  geom_smooth(method = "lm", color = "#FA7800") +
  labs(title = "median price by age")


ggplot(data, aes(x = EffectiveYear, y = price))+
  geom_line(stat = "summary", fun = "median") +
  geom_smooth(method = "lm", color = "#FA7800") +
  labs(title = "median price by year of last major renovation")

ggplot(data, aes(x = EffectiveYear, y = price))+
  geom_line(stat = "summary", fun = "median") +
  geom_smooth(method = "lm", color = "#FA7800") +
  labs(title = "median price by year of last major renovation")


# select numeric variables for correlation matrix
numericVars <- select_if(st_drop_geometry(finalData), is.numeric) %>%
  dplyr::select(
    # omit for more legible chart
    -toPredict,
    -MUSA_ID) %>%
  na.omit()

# create numeric variable correlation matrix and convert to data frame
corMatrix <- cor(numericVars)
corDF <- as.data.frame(as.table(corMatrix)) %>%
  rename(Cor = Freq)

# review numeric variables most correlated with price
corPrice <- filter(corDF, Var1 == "price")
corLogPrice <- filter(corDF, Var1 == "logPrice") # stronger correlations