# ---
# MUSA 508: Hedonic Home Price Prediction
# Step 2: Modeling
# Ericson, E. & León, A.
# ---

# --- SETUP --

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

# --- EXPLORATORY ANALYSIS ---

# set data set to explore
expData <- finalData # UPDATE WHEN CHECKING NEW DATA 

# plot correlation of individual variables with home values
# TODO: Try with different variables
st_drop_geometry(expData) %>% 
  dplyr::select(price, qualityNum) %>% # REPLACE WITH VARIABLES TO CHECK
  pivot_longer(cols = !price, names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value, price)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", color = "#FA7800") +
    facet_wrap(~Variable, ncol = 4, scales = "free") +
    labs(title = "Price as a function of continuous variables")

# select numeric variables for correlation matrix
numericVars <- select_if(st_drop_geometry(expData), is.numeric) %>%
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

corFinishedSF <- filter(corDF, Var1 == "TotalFinishedSF")
corQuality <- filter(corDF, Var1 == "qualityNum")

# generate correlation matrix chart for numeric variables
ggcorrplot(
  round(cor(numericVars), 1),
  p.mat = cor_pmat(numericVars),
  show.diag = TRUE,
  colors = c("#25cb10", "#ffffff", "#fa7800"),
  type = "lower",
  insig = "blank",
  lab = TRUE
) +
  labs(title = "Correlation across numeric variables")


# select non-numeric variables to convert to dummies
nonNumericVars <- select_if(st_drop_geometry(expData), Negate(is.numeric))

# convert categorical variables to dummies
dummyVars <- dummy.data.frame(nonNumericVars)

# add log(price) column to dummies
dummiesWithLogPrice <- cbind(expData$logPrice, dummyVars)

# create dummy variable correlation matrix as data frame
dummyCorDF <- as.data.frame(as.table(cor(dummiesWithLogPrice)))

# review dummy variables most correlated with price
dummyCorLogPrice <- filter(dummyCorDF, Var1 == "expData$logPrice")%>%
  rename(Cor = Freq)

# combine numeric and dummy variables
allVars <- cbind(numericVars, dummyVars)

# generate correlation matrix chart for dummy variables
# NOTE: only legible when exported as >5000px wide PNG
ggcorrplot(
  round(cor(dummyVars), 1),
  p.mat = cor_pmat(dummyVars),
  show.diag = TRUE,
  colors = c("#25cb10", "#ffffff", "#fa7800"),
  type = "lower",
  insig = "blank"
) +
  labs(title = "Correlation across categorical dummy variables")

# try a linear regression

expDataReg <- lm(logPrice ~ .,
                    data = st_drop_geometry(expData) %>%
                      dplyr::select(-toPredict, -MUSA_ID, -price)
)
summary(expDataReg)





# --- MODEL ESTIMATION & VALIDATION ---

regData <- finalData %>% filter(toPredict == 0, MUSA_ID != 1397) # UPDATE WHEN RUNNING NEW MODEL

# regData <- dplyr::select(regData, -city, -subcommunity)

# Split data into training (75%) and validation (25%) sets
inTrain <- createDataPartition(
  y = paste(
    regData$constMat, 
    regData$basement, 
    regData$acType, 
    regData$heatingType, 
    regData$extWall, 
    regData$extWall2, 
    regData$roofType,
    regData$tractID
  ),
  p = 0.75, list = FALSE)

homes.training <- regData[inTrain,]
homes.test <- regData[-inTrain,]

# Estimate model on training set
reg.training <- lm(logPrice ~ .,
                   data = st_drop_geometry(regData) %>%
                     dplyr::select(-toPredict, -MUSA_ID, -price)
)
                     
summary(reg.training)

# Calculate MAE and MAPE
homes.test <- homes.test %>%
  mutate(
    logPrice.Predict = predict(reg.training, homes.test),
    price.Predict = exp(logPrice.Predict),
    price.Error = price.Predict - price,
    price.AbsError = abs(price.Predict - price),
    price.APE = (abs(price.Predict - price)/price.Predict)    
  )

mean(homes.test$price.AbsError)
mean(homes.test$price.APE)

# TODO: Plot distribution of prediction errors

hist(homes.test$price.Error, breaks = 50)
hist(homes.test$price.AbsError, breaks = 50)
hist(homes.test$price.APE, breaks = 50)

# TODO: Perform k-fold cross-validation using caret package

fitControl <- trainControl(method = "cv", number = 100)
set.seed(825)

reg.cv <- 
  train(
    logPrice ~ .,
    data = st_drop_geometry(homes.training) %>%
      dplyr::select(-toPredict, -MUSA_ID, -price),
    method = "lm", 
    trControl = fitControl, 
    na.action = na.pass
  )

reg.cv

# TODO: Plot distribution of MAE

allMAE <- reg.cv$resample[,3]
hist(allMAE, breaks = 50)
max(allMAE)
sd(allMAE)

# TODO: Test generalizability across contexts (e.g. race, income)