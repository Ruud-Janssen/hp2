library(tidyverse)
library(glmnet)
library(gbm3)
library(caret)
library(C50)
library(mlbench)
library(data.table)
library(dummies)
library(ICC)

# columns <- c(
#   "numeric", 
#   rep("character", 2),
#   rep("numeric", 2),
#   rep("character", 10),
#   rep("numeric", 3),
#   rep("character", 3),
#   "numeric",
#   rep("character", 1), #foundation
#   rep("numeric", 4),
#   rep("character", 3),
#   rep("numeric", 10),
#   rep("character", 1), #GarageType
#   rep("numeric", 7),
#   "character",
#   "numeric",
#   "character",
#   rep("numeric", 22))
# 
# allData <- fread("input/House Prices Kaggle_preprocessed.csv", data.table = FALSE, header = TRUE, sep = ",", colClasses = columns)


allData <- read_csv("input/House Prices Kaggle_preprocessed.csv")

##Removing variables containing only NAs
allData <- allData %>% select(-HouseDirectModifiedAfterBuilt, -HouseDirectSoldAfterBuilt, -HouseIsModified, -GarageDirectBuilt)

#Removing useless variables
allData <- allData %>% select(-MiscFeature)



namesTrain   <- names(allData)
namesFactors <- namesTrain[endsWith(namesTrain, ".n")]
namesChars   <- namesTrain[sapply(allData, is.character)]
namesNAs     <- namesTrain[sapply(allData, function(x) {sum(is.na(x)) > 0})]


allData[namesFactors] <- lapply(allData[namesFactors], factor, ordered = T)
allData[namesChars]   <- lapply(allData[namesChars], factor)

allData <- allData %>% 
  mutate(OverallQual   = factor(OverallQual, ordered = T),
         OverallCond   = factor(OverallCond, ordered = T),
         SaleCondition = factor(SaleCondition, ordered = T),
         MSSubClass    = factor(MSSubClass)
  )


##Replacing the NAs in a quick, dirty way
RowsWithNAs <- which(rowSums(is.na(allData)) > 0)

allData[RowsWithNAs[1:2], namesNAs[1]] <- 1
allData[RowsWithNAs[1:2], namesNAs[2]] <- 0
allData[RowsWithNAs[3], namesNAs[3]]   <- 2

# Renaming variables because ranger can not handle column names starting with numbers
allData$FirstFlrSf    <- allData$`1stFlrSF`
allData$`1stFlrSF`    <- NULL
allData$secondFlrSf   <- allData$`2ndFlrSF`
allData$`2ndFlrSF`    <- NULL
allData$thirdSsnPorch <- allData$`3SsnPorch`
allData$`3SsnPorch`   <- NULL

train <- allData %>% filter(SalePriceLog != 0)
test  <- allData %>% filter(SalePriceLog == 0)

##Find the correlations between SalePriceLog and the numeric variables
absCorrelations <- abs(cor(x = train[, sapply(train, is.numeric)] %>% select(-SalePriceLog), y = train %>% select(SalePriceLog)))
absCorrelations <- tibble(variables = as.character(row.names(absCorrelations)), absCor = as.numeric(absCorrelations))
absCorrelations <- absCorrelations %>% arrange(absCor)

#Remove 9 variables with the lowest correlation (all variables with a correlation up to random variable ID)
train   <- train[, !(names(train) %in% c(absCorrelations$variables[1:9])) ]
allData <- allData[, !(names(allData) %in% c(absCorrelations$variables[1:9]))]

##Find the correlations between SalePriceLog and the factor variables
ICCS <- NULL

for (i in namesFactors) {
  ICCS <- c(ICCS, ICCbare(x = i, y = SalePriceLog, data = train))
}

ICCt <- tibble(variables = namesFactors, ICC = abs(ICCS))
ICCt <- ICCt %>% arrange(ICC)

train   <- train %>% select(-LandSlope)
allData <- allData %>% select(-LandSlope)

##Starting with Random Forest
set.seed(43413)

fitControl <- trainControl(method = "oob", 
                           returnResamp = "all")

# Choose the features and classes
xTrain <- as.data.frame(train %>% select(-SalePriceLog))
yTrain <- train$SalePriceLog

#grid <- expand.grid(.mstop = c(5000, 10000, 15000), .maxdepth=c(10,15,20), .nu=c(0.001, 0.002, 0.005))
grid <- expand.grid(mtry = 1 : 14 * 5)

mdl <- train(x = xTrain, y = yTrain, trControl = fitControl, tuneGrid = grid, method = "ranger", importance = "impurity")

mdl
min(mdl$results$RMSE)
mdl$finalModel
impVariables <- importance(mdl$finalModel) 
impVariables <- sort(impVariables, decreasing = T)

##Top 87 variables selected
xTrain87 <- xTrain %>% select(names(impVariables[1 :87]))

grid87 <- expand.grid(mtry = 1 : 14 * 3)

mdl87 <- train(x = xTrain87, y = yTrain, trControl = fitControl, tuneGrid = gridNew, method = "ranger", importance = "impurity")

mdl87
min(mdl87$results$RMSE)
mdl87$finalModel

##Predictions Mdl
xTest <- as.data.frame(test %>% select(-SalePriceLog))
testPredictionsMdl <- exp(predict(mdl, newdata = xTest))

##ElasticNet
#train <- train %>% select(names(impVariables[1 :80]), SalePriceLog)
trainElasticNet  <- dummy.data.frame(data.frame(train),sep = ".") %>% as_tibble()

ctrlElasticNet <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

gridElasticNet <- expand.grid(alpha = seq(0, 0.5, 0.01), lambda = 0.5 * 10 ^ seq(-1, -2, length = 100))
mdlElasticNet  <- train(SalePriceLog ~., data = trainElasticNet, 
                        method = "glmnet", family = "gaussian", 
                        trControl = ctrlElasticNet, tuneGrid = gridElasticNet)
#mdlElasticNet
min(mdlElasticNet$results$RMSE)
mdlElasticNet$bestTune

##Predictions ElasticNet
xTestElasticNet           <- dummy.data.frame(data.frame(test),sep = ".") %>% as_tibble()
xTestElasticNet           <- xTestElasticNet %>% 
  mutate(HouseStyle.2.5Fin = 0,
         RoofMatl.ClyTile  = 0, 
         RoofMatl.Membran  = 0, 
         RoofMatl.Metal    = 0, 
         RoofMatl.Roll     = 0, 
         Heating.Floor     = 0, 
         Heating.OthW      = 0, 
         Electrical.1      = 0, 
         Electrical.Mix    = 0, 
         MiscFeature.TenC  = 0)
testPredictionsElasticNet <- exp(predict(mdlElasticNet, newdata = xTestElasticNet))

results <- data.frame(Id = xTest$Id, SalePrice = testPredictionsElasticNet)

write.csv(results, file = "Submission.csv", row.names=FALSE)


##Stochastic Gradient Boosting
ctrlGBM <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

gridGBM <- expand.grid(n.trees = 7000, interaction.depth = 4:6, shrinkage = 0.006 + 0.001 * 1 : 3, n.minobsinnode = c(5, 10, 15))

mdlGBM  <- train(SalePriceLog ~., data = trainElasticNet, 
                 method = "gbm", distribution = "gaussian", 
                 tuneGrid = gridGBM, trControl = ctrlGBM)



# shrinkage  n.trees  RMSE       Rsquared 
# 0.008      6000     0.1234184  0.9051609
# 0.008      7000     0.1233277  0.9053028
# 0.008      8000     0.1234059  0.9052046
# 0.010      6000     0.1233815  0.9052745
# 0.010      7000     0.1234521  0.9051770
# 0.010      8000     0.1235645  0.9049929
# 0.012      6000     0.1234010  0.9052634
# 0.012      7000     0.1235172  0.9050871
# 0.012      8000     0.1236401  0.9049263
# 
# Tuning parameter 'interaction.depth' was held constant at a value of 4
# Tuning parameter 'n.minobsinnode' was held constant at a value of 10
# RMSE was used to select the optimal model using  the smallest value.
# The final values used for the model were n.trees = 7000, interaction.depth = 4, shrinkage = 0.008 and n.minobsinnode = 10.

testPredictionsGBM <- exp(predict(mdlGBM, newdata = xTestElasticNet))
results            <- data.frame(Id = xTest$Id, SalePrice = testPredictionsGBM)
write.csv(results, file = "SubmissionGBM.csv", row.names=FALSE)
