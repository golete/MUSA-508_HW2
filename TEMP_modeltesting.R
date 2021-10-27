regData <- finalData # UPDATE WHEN RUNNING NEW MODEL

# Split data into training (75%) and validation (25%) sets
inTrain <- createDataPartition(
  y = paste(
    regData$constMat, 
    # regData$basement, 
    # egData$acType, 
    regData$heatingType, 
    # regData$extWall, 
    regData$extWall2, 
    # regData$roofType,
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

allMAE <- reg.cv$resample[,3]
hist(allMAE, breaks = 50)
max(allMAE)
sd(allMAE)