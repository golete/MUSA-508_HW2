# ---
# MUSA 508: Hedonic Home Price Prediction
# Step 1: Data Wrangling
# Ericson, E. & León, A.
# ---

# --- SETUP --

# load libraries
library("tidyverse")
library("tidycensus")
library("sf")
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(kableExtra)
library(jtools)     # for regression model plots
library(ggstance) # to support jtools plots
library(mapview)

# Working setup
options(scipen = 999)
g <- glimpse
m <- mapview


# --- DATA WRANGLING ---

# A. HOME VALUE DATA

# read in home value data

data <- st_read("studentData.geojson")

st_crs(data$geometry)
# ESPG 4326 in meters


varsA <- c('price',
           'year_quarter',      # Time of sale
           'designCode',        # Type of property
           'designCodeDscr',    
           'qualityCode',       # Condition
           'qualityCodeDscr',
           'ConstCodeDscr',     # Materiality
           'builtYear',         # Year Built
           'EffectiveYear',     # Year of Last Major Renovation
           'carStorageSF',      # Garage area
           'nbrRoomsNobath',    # Number of rooms (excluding bathrooms)
           'nbrBedRoom',        # Number of bedrooms
           'nbrFullBaths',      # Number of full baths
           'nbrThreeQtrBaths',   # Number of three quarter baths
           'nbrHalfBaths',      # Number of half baths
           'TotalFinishedSF',   # Total area (sq foot)
           'MUSA_ID',
           'geometry')


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

# designCodeDscr
#   "1 Story - Ranch"
#   "Bi-level"
#   "Split-level"
#   "2-3 Story"
#   "MULTI STORY- TOWNHOUSE"

# qualityCodeDscr
#   "EXCEPTIONAL 2 ", "EXCEPTIONAL 1 ",
#   "EXCELLENT++ "   "EXCELLENT + ", "EXCELLENT "
#   "VERY GOOD ++ ", "VERY GOOD + ", "VERY GOOD ",
#   "GOOD ++ ", "GOOD + ", "GOOD ",
#   "AVERAGE ++ ", "AVERAGE + ", "AVERAGE ",
#   "FAIR ","LOW "

# ConstCodeDescr
#   "Frame"
#   "Masonry"
#   "Wood"
#   "Brick"
#   "Concrete"
#   "Veneer"
#   "Precast" 

houseData <- 
  # exclude extreme outlier listed as sold for $31.5 million; 
  # listing strongly suggests incorrectly entered: https://www.zillow.com/homedetails/3335-Talisman-Ct-APT-C-Boulder-CO-80301/13222117_zpid/
  filter(data, price < 10000000) %>%
  # create features from existing variables
  mutate(
    nbrRooms = nbrRoomsNobath+nbrFullBaths+(nbrThreeQtrBaths+nbrHalfBaths)/2,
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


house <- houseData %>%
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


hist(house$nbrRooms)



# B. BOUNDARY DATA (Neighborhoods, school districts, city, etc.)

# B1. Boulder county boundaries

countylimits <- st_read('County_Boundary.geojson') %>%
  select(OBJECTID, geometry)
st_crs(countylimits)
# CRS: EPSG: 4326, WGS84, metres

munis <- st_read('Municipalities.geojson') %>%
  select(ZONEDESC, geometry)
st_crs(munis)
# CRS: EPSG: 4326, WGS84, metres


# B2. Boulder city and other cities/zones boundaries

# To solve problem with spherical, ask for implications ****
sf::sf_use_s2(FALSE)

zones <- st_read('Zoning_-_Zoning_Districts.geojson') %>%
  select(ZONEDESC, geometry) %>%
  filter(ZONEDESC != 'Boulder') %>%
  group_by(ZONEDESC) %>%
  rename(SUBCOMMUNITY = ZONEDESC) %>%
  summarize(geometry = st_union(geometry))
st_crs(zones)
# CRS: EPSG: 4326, WGS84, metres

# Union the polygons that are not in boulder City limits, just in case
notCity <- zones %>% st_union
mapview(notCity)

# B3. Boulder City Zoning Districts

districts <- st_read('Zoning_Districts.geojson') %>%
  select(OBJECTID, ZONING, ZNDESC, geometry)
st_crs(districts)
# CRS: EPSG: 4326, WGS84, metres

# Load the subcommunities / neighborhoods rough boundaries
subcomms <-  st_read('Subcommunities.geojson')
st_crs(subcomms)
# CRS: EPSG: 4326, WGS84, metres

# Join the region zoning polygons with the subcommunities polygons and union
cityHoods <- st_join(districts, subcomms, largest=TRUE) %>%
  select(SUBCOMMUNITY, geometry) %>%
  group_by(SUBCOMMUNITY) %>%
  summarize(geometry = st_union(geometry))

mapview(zones)
mapview(cityHoods)

neighborhoods <- rbind(zones, cityHoods)
m(neighborhoods)

# Subset the generic zones as a separate map just if needed
genericZones <- c('Business',
                  'Commercial',
                  'Economic Development',
                  'Estate Residential',
                  'General Industrial',
                  'Light Industrial',
                  'Manufactured Home',
                  'Mountain Institutional',
                  'Multiple Family',
                  'Rural Residential',
                  'Suburban Residential',
                  'Transitional')
uZones <- zones %>%
  filter(SUBCOMMUNITY %in% genericZones)
m(uZones)
# COMPARE WITH NOT-CITY boundaries



# C. CENSUS DATA

# set API key
# NOTE: Set overwrite to "false" to prevent overwriting my own key locally -EE
census_api_key("e79f3706b6d61249968c6ce88794f6f556e5bf3d", overwrite = FALSE)

year <- 2019
state <- 08
county <- 13

# review available variables
acsVariableList <- load_variables(year,"acs5",cache = TRUE)

# define variables to import
varsC <- c('B25003_001E', # Total housing units
           'B25003_002E', # Total owner-occupied
           'B25003A_001E', # Total white households
           'B25002_003E', # Total vacant housing units
           'B17026_001E', # Ratio of income to poverty Total
           'B17026_002E', # Ratio of income to poverty under 0.50
           'B17026_003E', # Ratio of income to poverty .50 to .74
           'B17026_004E', # Ratio of income to poverty .75 to .99
           'B17026_005E', # Ratio of income to poverty 1.00 to 1.24
           'B17026_006E', # Ratio of income to poverty 1.25 to 1.49
           'B17026_007E', # Ratio of income to poverty 1.50 to 1.74
           'B17026_008E', # Ratio of income to poverty 1.75 to 1.84
           'B17026_009E', # Ratio of income to poverty 1.85 to 1.99
           'B17026_010E', # Ratio of income to poverty 2.00 to 2.99
           'B17026_011E', # Ratio of income to poverty 3.00 to 3.99
           'B17026_012E', # Ratio of income to poverty 4.00 to 4.99
           'B17026_013E', # Ratio of income to poverty 5.00 and more
           'B15003_001E', # Total education
           'B15003_022E', # Bachelor's degree
           'B15003_023E', # Master's degree
           'B15003_024E', # Professional school degree
           'B15003_025E'  # Doctorate degree
           )


# import variables
tracts <- 
  get_acs(geography = "tract",
          variables = varsC,
          year = year,
          state = state,
          county = county,
          geometry = T,
          output = 'wide') %>%
  dplyr::select(-ends_with('M')) %>%
  rename(HHtotal = B25003_001E,
         HHownerOc = B25003_002E,
         HHwhite = B25003A_001E, # Total white households
         HHvacant = B25002_003E, # Total vacant housing units
         EduTotal = B15003_001E, # Total education
         EduBachs = B15003_022E, # Bachelor's degree
         EduMasts = B15003_023E, # Master's degree
         EduProfs = B15003_024E, # Professional school degree
         EduDocts = B15003_025E # Doctorate degree
         ) %>%
  mutate(PCTHHowner = HHownerOc/HHtotal) %>%
  mutate(PCTHHwhite = HHwhite/HHtotal) %>%
  mutate(PCT25yrHighEdu = (EduBachs+EduMasts+EduProfs+EduDocts)/EduTotal) %>%
  mutate(PCTbel125pov = (B17026_002E+B17026_003E+B17026_004E+B17026_005E)/B17026_001E) %>%
  mutate(PCT125185pov = (B17026_006E+B17026_007E+B17026_008E)/B17026_001E) %>%
  mutate(PCT185300pov = (B17026_009E+B17026_010E)/B17026_001E) %>%
  select(-HHownerOc,-HHwhite,-starts_with('Edu'), -starts_with('B17026'))

st_crs(tracts$geometry) # CRS: ESPG 4269, NAD84 metres



# D. OTHER DATA (CRIME, FEMA, etc.)

# D1. Wildfire history data

wildfires <-
  st_read('Wildfire_History.geojson') %>%
  select(-Shapearea, -Shapelen, -LABELNAME, -STARTDATE)

# ENDDATE -- FILTER to only fires that happened after 2000? 2010?

st_crs(wildfires$geometry) # CRS: EPSG 4326, metres

# mapview(wildfires)


# D2. CHAMP floodplain maps

floodplains <- 
  st_read('Floodplain_-_BC_Regulated.geojson') %>%
  select(FLD_AR_ID, # Primary key for table lookup.
         SFHA_TF, #Special Flood Hazard Area. If the area is within the SFHA, this field would be true any area that is coded for any A or V zone flood areas. It should be false for all other flood zone areas. Acceptable values for this field are listed in the D_TrueFalse table.
         FLD_ZONE, #Flood Zone. This is a flood zone designation. These zones are used by FEMA to designate the SFHAs and for Acceptable values for this field are listed in the D_Zone table. 
         ZONE_SUBTY, #Flood Zone Type. Flood Zone areas that will remain free of development to moderate increases in flood heights due to encroachment on the floodplain. Acceptable values for this field are listed in the D_ ZONE_SUBTY table. 
         geometry)
st_crs(floodplains) # CRS: EPSG 4326, WGS 84, metres

# METADATA from https://www.hsdl.org/?view&did=7705

# SFHA means Special Flood Hazard Area
#  TRUE, FALSE or NA

# FLD_ZONE        High-risk areas have at least a 1% annual chance of flooding.  This flood is also referred to as the Base Flood.  Flood insurance is required for structures in these high-risk areas if they have a federally-backed mortgage.
#  ZONE AE        Area inundated by the Base Flood with Base Flood Elevations determined.
#  ZONE AH        Area inundated by the Base Flood with flood depths of 1 to 3 feet (usually areas of ponding); Base Flood Elevations determined.
#  ZONE AO        Area inundated by the Base Flood with flood depths of 1 to 3 feet (usually sheet flow on sloping terrain); average depths determined. For areas of alluvial fan flooding, velocities are also determined.
#  ZONE X (0.2%)  This zone designation is for multiple risks including areas of the 0.2% annual chance flood; areas of the 1% annual chance flood with average depths of less than 1 foot or with drainage areas less than 1 square mile; and areas protected by levees from the 1% annual chance flood.
#  ZONE X         Areas determined to be outside the 0.2% annual chance floodplain. 

# ZONE_SUBTY
#  0500 0.2 PCT ANNUAL CHANCE FLOOD HAZARD FIRM
#  1000 AREA WITH REDUCED FLOOD RISK DUE TO LEVEE FIRM
#  1100 FLOODWAY 
#  2000 AREA OF MINIMAL FLOOD HAZARD 

mapview(floodplains$geometry)



# E. EXPERIMENTAL DATA

# E1. Whole Foods locations

wholefoodsData <- st_read("wholefoodsmarkets_boulderCO.csv")

wholefoods <- st_as_sf(wholefoodsData, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 3857) %>%
  dplyr::select(-phone, -address) %>%
  st_buffer(5000) %>%
  st_union()

m(wholefoods)

# E2. Marijuana dispensaries
marijuana <- st_read("Marijuana_Establishments.geojson") %>%
  st_transform(crs = 3857) %>%
  dplyr::select(OBJECTID, Type, geometry) %>%
  st_buffer(2414) %>%
  st_union()

m(marijuana)

st_crs(marijuana) # CRS: EPSG:4326, WGS 84



# --- EXPORT ---

# TODO: Add code to export final analysis data set for modeling script