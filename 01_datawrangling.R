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

# --- DATA WRANGLING ---

# 1. HOME VALUE DATA

# read in home value data
data <- st_read("studentData.geojson")

# TODO: Figure out projection

# BOUNDARY DATA

# TODO: Boulder city and county boundaries
# TODO: Neighborhood boundaries
# TODO: School district boundaries

# 2. CENSUS DATA

# set API key
# NOTE: Set overwrite to "false" to prevent overwriting my own key locally -EE
census_api_key("e79f3706b6d61249968c6ce88794f6f556e5bf3d", overwrite = FALSE)

# review available variables
acsVariableList <- load_variables(year,"acs5",cache = TRUE)

# TODO: Select variables for analysis

# define variables to import
acsVars <- c('B25003_001E', # Total housing units
             'B25003_002E', # Total owner-occupied
             'B25003A_001', # Total white households
             'B25003A_002', # Total white owner occupied households
             'B25002_003E') # Total vacant housing units

# import variables
tracts <- 
  get_acs(geography = "tract",
          variables = acsVars,
          year = year,
          state = state,
          county = county,
          geometry = T,
          output = 'wide') #%>%
# st_transform('ESRI:102635')

# TODO: Figure out projection

# 3. OTHER DATA

# TODO: Wildfire history data
# TODO: FEMA flood insurance maps
# TODO: Figure out others

# 4. EXPERIMENTAL DATA

# Whole Foods locations
wholefoods <- st_read("wholefoodsmarkets_boulderCO.csv") %>%
  dplyr::select(-phone) %>%
  st_as_sf(coords= c('lat','lon'), crs = 4269)

# TODO: Assessor mill levy (property tax rate)
# TODO: Airport influence?
# TODO: Figure out others

# --- EXPORT ---

# TODO: Add code to export final analysis data set for modeling script