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

# --- DATA WRANGLING ---

# A. HOME VALUE DATA

# read in home value data

data <- st_read("studentData.geojson")

st_crs(data$geometry)
# ESPG 4326 in meters

unique(data$'ConstCodeDscr')

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

house <- data %>%
  dplyr::select(varsA) %>%
  mutate(nbrRooms = nbrRoomsNobath+nbrFullBaths+(nbrThreeQtrBaths+nbrHalfBaths)/2)
  
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

zones <- st_read('Zoning_-_Zoning_Districts.geojson') %>%
  select(ZONEDESC, geometry) 
st_crs(zones)
# CRS: EPSG: 4326, WGS84, metres

zonelist <-  unique(zones$ZONEDESC)

businessZone <- zones %>%
  filter(ZONEDESC == zonelist[1])
subResZone <- zones %>%
  filter(ZONEDESC == zonelist[2])
commercialZone <- zones %>%
  filter(ZONEDESC == zonelist[3])
transZone <- zones %>%
  filter(ZONEDESC == zonelist[4])
manufZone <- zones %>%
  filter(ZONEDESC == zonelist[5])
lightIndZone <- zones %>%
  filter(ZONEDESC == zonelist[6])
multFamZone <- zones %>%
  filter(ZONEDESC == zonelist[7])
econDevZone <- zones %>%
  filter(ZONEDESC == zonelist[8])
industZone <- zones %>%
  filter(ZONEDESC == zonelist[9])
aggrZone <- zones %>%
  filter(ZONEDESC == zonelist[10])
rurResZone <- zones %>%
  filter(ZONEDESC == zonelist[11])
estateResZone <- zones %>%
  filter(ZONEDESC == zonelist[12])
forestZone <- zones %>%
  filter(ZONEDESC == zonelist[13])
Niwot1Zone <- zones %>%
  filter(ZONEDESC == zonelist[14])
Niwot2Zone <- zones %>%
  filter(ZONEDESC == zonelist[15])
MountainZone <- zones %>%
  filter(ZONEDESC == zonelist[16])

Louisville <- zones %>%
  filter(ZONEDESC == zonelist[17])
Ward <- zones %>%
  filter(ZONEDESC == zonelist[18])
Jamestown <- zones %>%
  filter(ZONEDESC == zonelist[19])
Historic <- zones %>%
  filter(ZONEDESC == zonelist[20])
Nederland <- zones %>%
  filter(ZONEDESC == zonelist[21])
Boulder <- zones %>%
  filter(ZONEDESC == zonelist[22])
Erie <- zones %>%
  filter(ZONEDESC == zonelist[23])
Lafayette <- zones %>%
  filter(ZONEDESC == zonelist[24])
Longmont <- zones %>%
  filter(ZONEDESC == zonelist[25])
Lyons <- zones %>%
  filter(ZONEDESC == zonelist[26])
Superior <- zones %>%
  filter(ZONEDESC == zonelist[27])

Municipalities<- zones %>%
  filter(ZONEDESC %in% zonelist[16:27])
unincorporatedZones<- zones %>%
  filter(ZONEDESC %in% zonelist[1:16])
mapview(unincorporatedZones)



# B3. Boulder City Zoning Districts

cityHoods <- st_read('Zoning_Districts.geojson') %>%
  select(OBJECTID, ZONING, ZNDESC, geometry)
st_crs(cityHoods)
# CRS: EPSG: 4326, WGS84, metres

cx <- cityHoods %>% select(ZONING, geometry)
mapview(cx)


unique(cityHoods$ZONINGDISTPURPOSE)
glimpse(cityHoods)
mapview(cityHoods)


subcomms <-  st_read('Subcommunities.geojson')
mapview(subcomms)
st_crs(subcomms)
# CRS: EPSG: 4326, WGS84, metres

centroids <- st_centroid(cityHoods)

abcccc <- st_covered_by(cityHoods, subcomms)

# TODO: School district boundaries



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


# D3. FEMA flood insurance map



# TODO: Figure out others



# E. EXPERIMENTAL DATA

# E1. Whole Foods locations
wholefoods <- st_read("wholefoodsmarkets_boulderCO.csv") %>%
  dplyr::select(-phone) %>%
  st_as_sf(coords= c('lat','lon'), crs = 4269)

# TODO: Assessor mill levy (property tax rate)
# TODO: Airport influence?
# TODO: Figure out others

# --- EXPORT ---

# TODO: Add code to export final analysis data set for modeling script