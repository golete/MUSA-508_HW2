# load libraries
library("tidyverse")
library("tidycensus")
library("sf")

# prevent scientific notation
options(scipen = 999)

# read in house data
data <- st_read("studentData.geojson")

# examine distribution of known prices
knownPrice <- filter(data, toPredict == 0)

# min(knownPrice$price) # $10,000
# max(knownPrice$price) # $31,500,000
# mean(knownPrice$price) # $749,112.9
# mean(cleanerData$price) # $746,382.6
# median(knownPrice$price) # 605,750
# hist(cleanerData$price, breaks = 100)
# hist(log(knownPrice$price), breaks = 100)
# hist(log(cleanerData$price), breaks = 100)

# hist(cleanerData$price)
# hist(log(knownPrice$price))
# hist(log(cleanerData$price))

newVarData <- 
  # exclude extreme outlier listed as sold for $31.5 million; 
  # listing strongly suggests incorrectly entered: https://www.zillow.com/homedetails/3335-Talisman-Ct-APT-C-Boulder-CO-80301/13222117_zpid/
  filter(data, price < 10000000) %>%
  # create features from existing variables
  mutate(
    # calculate quarter sold
    quarterSold = str_sub(year_quarter, -1, -1),
    # calculate log of price to normalize positive skew
    logPrice = log(price),
    # convert building and section numbers to dummies
    manyBuildings = if_else(bld_num > 1, 1, 0),
    manySections = if_else(section_num > 1, 1, 0),
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
    # try collapsing quality categories
    # NOTE: still not at all normal, but see if more predictive
    qualityNum2 = case_when(
      qualityCode == 10 ~ 1, # "LOW"
      qualityCode == 20 ~ 2, # "FAIR"
      between(qualityCode, 30, 32) ~ 3, # "AVERAGE", "AVERAGE +", "AVERAGE ++"
      between(qualityCode, 40, 42) ~ 4, # "GOOD", "GOOD +", "GOOD ++"
      between(qualityCode, 50, 52) ~ 5, # "VERY GOOD", "VERY GOOD +", "VERY GOOD ++"
      between(qualityCode, 60, 62) ~ 6, # "EXCELLENT", "EXCELLENT +", "EXCELLENT ++"
      between(qualityCode, 70, 80) ~ 7 # "EXCEPTIONAL 1", "EXCEPTIONAL 2"
    ),
    # recode missing construction material values
    constMat = case_when(
      ConstCode == 0 ~ "Missing",
      ConstCode == 300 ~ "Unspecified",
      ConstCode > 300 ~ as.character(ConstCodeDscr)
    ),
    # calculate age and effective age (time since last major renovation)
    # TODO: Decide whether to recode negative age (homes sold before construction) as 0
    # TODO: Try effectiveAge with negatives recoded as 0, plus new construction dummy?
    age = year - builtYear,
    effectiveAge = year - EffectiveYear,
    effectiveAge2 = if_else(year <= builtYear, 0, year - EffectiveYear), # negative values recoded as 0
    newConstruction = if_else(year <= builtYear, 1, 0),
    # recode missing basement values
    basement = if_else(bsmtType == 0, "None", as.character(bsmtTypeDscr)),
    # try simpler basement categories
    basementDummy = if_else(bsmtType == 0, 0, 1),
    finishedBasement = if_else(str_detect(bsmtTypeDscr, " FINISHED"), 1, 0),
    walkOutBasement = if_else(str_detect(bsmtTypeDscr, "WALK"), 1, 0),
    # recode missing car storage values
    carStorage = if_else(carStorageType == 0, "None", as.character(carStorageTypeDscr)),
    # try simpler car storage categories
    carStorageSimpler = case_when(
      carStorageType == 0 ~ "None",
      str_detect(carStorageTypeDscr, "GARAGE") ~ "Garage",
      str_detect(carStorageTypeDscr, "CARPORT") ~ "Carport",
    ),
    # try as dummies
    carStorageDummy = if_else(carStorageType == 0, 0, 1),
    garageDummy = if_else(str_detect(carStorageTypeDscr, "GARAGE"), 1, 0),
    # calculate total bathrooms
    totalBaths = nbrFullBaths + (0.75 * nbrThreeQtrBaths) + (0.5 * nbrHalfBaths),
    totalBaths2 = nbrFullBaths + nbrThreeQtrBaths + (0.5 * nbrHalfBaths),
    totalBathOrShower = nbrFullBaths + nbrThreeQtrBaths,
    # recode missing a/c values
    acType = case_when(
      is.na(Ac) ~ "None",
      Ac == 200 ~ "Unspecified", # Code with no description
      Ac >= 210 ~ as.character(AcDscr) # "Attic Fan", "Evaporative Cooler", "Whole House" unchanged
    ),
    # create binary a/c feature to exclude fans, coolers, and unspecified
    acDummy = replace_na(if_else(Ac == 210, 1, 0), 0),
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

cleanerData <- newVarData %>%
  # drop unnecessary columns and columns with too much missing data
  dplyr::select(
    # same for all
    -bldgClass,
    -bldgClassDscr,
    -status_cd,
    # not needed
    -saleDate,
    -address,
    -designCode, # see designCodeDscr
    # too much missing data
    -Stories,
    -UnitCount,
    # recoded
    # -year_quarter, # compare with year and quarter as separate features?
    -bld_num,
    -section_num,
    -qualityCode,
    -qualityCodeDscr,
    -ConstCode,
    -ConstCodeDscr,
    -builtYear,
    -EffectiveYear,
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

# test recodes 

# qualityTest <- dplyr::select(newVarData, qualityCode, qualityCodeDscr, qualityNum, qualityNum2)
# acTest <- dplyr::select(newVarData, Ac, AcDscr, acType, acDummy)
# ageTest <- dplyr::select(newVarData, year, builtYear, EffectiveYear, age, effectiveAge, effectiveAge2, newConstruction)
# buildingsTest <- dplyr::select(newVarData, bld_num, manyBuildings, section_num, manySections)
# constMatTest <- dplyr::select(newVarData, ConstCode, ConstCodeDscr, constMat)
# quarterTest <- dplyr::select(newVarData, year_quarter, quarterSold)
# basementTest <- dplyr::select(newVarData, bsmtType, bsmtTypeDscr, basement, basementDummy, finishedBasement, walkOutBasement)
# carStorageTest <- dplyr::select(newVarData, carStorageType, carStorageTypeDscr, carStorage, carStorageSimpler, carStorageDummy, garageDummy)
# bathsTest <- dplyr::select(newVarData, nbrFullBaths, nbrThreeQtrBaths, nbrHalfBaths, totalBaths, totalBaths2, totalBathOrShower)
# wallsTest <- dplyr::select(newVarData, ExtWallPrim, ExtWallDscrPrim, ExtWallSec, ExtWallDscrSec, extWall, extWall2, IntWall, IntWallDscr, intWall)
#roofTest <- dplyr::select(newVarData, Roof_Cover, Roof_CoverDscr, roofType)