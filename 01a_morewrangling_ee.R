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

cheapHouses <- filter(knownPrice, price < 200000)
hist(cheapHouses$price, breaks = 20)

cleanerData <- 
  # exclude extreme outlier listed as $31.5 million; 
  # listing strongly suggests incorrectly entered: https://www.zillow.com/homedetails/3335-Talisman-Ct-APT-C-Boulder-CO-80301/13222117_zpid/
  filter(data, price < 10000000) %>%
  dplyr::select(-bldgClass, -bldgClassDscr) %>%
  mutate(
    # calculate log of price to normalize positive skew
    logPrice = log(price),
    # calculate age and effective age (time since last major renovation)
    # TODO: Decide whether to recode negative age (homes sold before construction) as 0
    # TODO: Try effectiveAge with negatives recoded as 0, plus new construction dummy (builtYear <= year)?
    age = year - builtYear,
    effectiveAge = year - EffectiveYear,
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
    # recode missing a/c values
    acType = case_when(
      Ac == NA ~ "None",
      Ac == 200 ~ "Unspecified", # Code with no description
      Ac >= 210 ~ as.character(Ac) # "Attic Fan", "Evaporative Cooler", "Whole House" unchanged
    ),
    # create binary a/c feature to exclude fans, coolers, and unspecified
    acDummy = replace_na(ifelse(Ac == 210, 1, 0), 0)
  )

# review affected columns to make sure recodes worked
qualityTest <- dplyr::select(cleanerData, qualityCode, qualityCodeDscr, qualityNum, qualityNum2)
acTest <- dplyr::select(cleanerData, Ac, AcDscr, acType, acDummy)
ageTest <- dplyr::select(cleanerData, year, builtYear, EffectiveYear, age, effectiveAge)
