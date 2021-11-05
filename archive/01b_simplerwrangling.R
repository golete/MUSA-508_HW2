library(tidyverse)
library(sf)

data <- st_read("studentData.geojson")

defaultFeaturesTemp <- data %>%
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
    roofType = if_else(is.na(Roof_Cover), "Missing", as.character(Roof_CoverDscr))
  )

defaultFeatures <- defaultFeaturesTemp %>%
  # exclude extreme outlier listed as sold for $31.5 million; 
  # listing strongly suggests incorrectly entered: https://www.zillow.com/homedetails/3335-Talisman-Ct-APT-C-Boulder-CO-80301/13222117_zpid/
  filter(price < 10000000) %>%
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
    -Roof_CoverDscr
  )