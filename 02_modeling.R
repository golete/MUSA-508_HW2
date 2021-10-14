# ---
# MUSA 508: Hedonic Home Price Prediction
# Step 2: Modeling
# Ericson, E. & León, A.
# ---

# --- SETUP --

# load libraries
library("tidyverse")
library("tidycensus")
library("sf")
library(spdep)
library(caret)
library(ckanr) # TODO: Determine if actually needed; used to read CKAN APIs
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(kableExtra)
library(jtools)     # for regression model plots
library(ggstance) # to support jtools plots

# avoid scientific notation
options(scipen = 999)

# --- EXPLORATORY ANALYSIS ---

# plot correlation of individual variables with home values
# TODO: Try with different variables
st_drop_geometry(data) %>% # TODO: Replace with analysis dataset
  dplyr::select(price, nbrBedRoom, nbrFullBaths, TotalFinishedSF) %>%
  pivot_longer(cols = !price, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value, price)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", color = "#FA7800") +
    facet_wrap(~Variable, ncol = 4, scales = "free") +
    labs(title = "Price as a function of continuous variables")

# TODO: Figure out what to do about extreme outlier (>$3 million house)

# select variables for correlation matrix
numericVars <- select_if(st_drop_geometry(data), is.numeric) %>%
  dplyr::select(
    # omit for code to run
    -UnitCount, 
    -Stories, 
    -Roof_Cover, 
    -ExtWallSec, 
    -Ac,
    # omit for more legible chart
    -toPredict,
    -MUSA_ID,
    -section_num,
    -bld_num) %>%
  na.omit()

# generate correlation matrix
# NOTE: no idea why background grid is misaligned
ggcorrplot(
  round(cor(numericVars), 1),
  p.mat = cor_pmat(numericVars),
  show.diag = TRUE,
  colors = c("#25cb10", "#ffffff", "#fa7800"),
  type = "lower",
  insig = "blank"
) +
  labs(title = "Correlation across numeric variables")
  
# TODO: Univariate regressions?

# --- FEATURE ENGINEERING ---

# TODO: Figure out if this should go in data wrangling script instead

# TODO: Calculate home age and effective age at sale

# --- FEATURE SELECTION ---

# TODO: Figure out if this organization even makes sense,
# considering everything is iterative
  

# --- MODEL ESTIMATION & VALIDATION ---

# TODO: Split data into training and validation sets
# TODO: Estimate model on training set
# TODO: Calculate MAE and MAPE
# TODO: Plot distribution of prediction errors

# TODO: Perform k-fold cross-validation using caret package
# TODO: Plot distribution of MAE

# TODO: Test generalizability across contexts (e.g. race, income)