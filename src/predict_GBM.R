library(ProjectTemplate)
load.project()


House.Prices.Kaggle.preprocessed[namesTrain_fac] <- lapply(House.Prices.Kaggle.preprocessed[namesTrain_fac], factor, ordered = T)
House.Prices.Kaggle.preprocessed[namesTrain_char] <- lapply(House.Prices.Kaggle.preprocessed[namesTrain_char], factor)

# train_hp_preprocessed[namesTrain_fac] <- lapply(train_hp_preprocessed[namesTrain_fac], factor, ordered = T)
# train_hp_preprocessed[namesTrain_char] <- lapply(train_hp_preprocessed[namesTrain_char], factor)
# 
# test_hp_preprocessed[namesTrain_fac] <- lapply(test_hp_preprocessed[namesTrain_fac], factor, ordered = T)
# test_hp_preprocessed[namesTrain_char] <- lapply(test_hp_preprocessed[namesTrain_char], factor)





















train <- House.Prices.Kaggle.preprocessed[House.Prices.Kaggle.preprocessed$Id <= 1460,]
#SaleCondition = factor(SaleCondition, ordered = T) ?? Bart


##Find the correlations between SalePriceLog and the numeric variables
absCorrelations <- abs(cor(x = train[, union(namesTrain_num, namesTrain_int)] %>% select(-SalePriceLog), y = train$SalePriceLog))
absCorrelations <- tibble(variables = as.character(row.names(absCorrelations)), absCor = as.numeric(absCorrelations))
absCorrelations <- absCorrelations %>% arrange(absCor)

#Remove 9 variables with the lowest correlation (all variables with a correlation up to random variable ID)
#looks a bit too simplistic
train   <- train[, !(names(train) %in% c(absCorrelations$variables[1:9])) ]
House.Prices.Kaggle.preprocessed <- House.Prices.Kaggle.preprocessed[, !(names(House.Prices.Kaggle.preprocessed) %in% c(absCorrelations$variables[1:9]))]


##Find the correlations between SalePriceLog and the factor variables
ICCS <- NULL

for (i in namesTrain_fac) {
  ICCS <- c(ICCS, ICCbare(x = i, y = SalePriceLog, data = train))
}

ICCt <- tibble(variables = namesTrain_fac, ICC = abs(ICCS))
ICCt <- ICCt %>% arrange(ICC)

train   <- train %>% select(-Functional.n)
#train   <- train %>% select(-LandSlope)
House.Prices.Kaggle.preprocessed <- House.Prices.Kaggle.preprocessed %>% select(-Functional.n)
#House.Prices.Kaggle.preprocessed <- House.Prices.Kaggle.preprocessed %>% select(-LandSlope)

rm("ICCS", "i", "ICCt", "absCorrelations", "train")















#Now we used some filter methods to reduce the features
#Now we use a wrapper method to reduce some features





#DON'T USE THIS GO TO BORUTA!!!!!!!!!!!!!!!!!!!!!!!!!

#One hot encoding in R
#dmy <- dummyVars(" ~ .", data = House.Prices.Kaggle.preprocessed, fullRank = T)
dmy <- dummyVars(as.formula(paste0(" ~ ", paste(namesTrain_char, collapse="+"))), data = House.Prices.Kaggle.preprocessed, fullRank = T)
House.Prices.Kaggle.preprocessed <- cbind(House.Prices.Kaggle.preprocessed, data.frame(predict(dmy, newdata = House.Prices.Kaggle.preprocessed)))
House.Prices.Kaggle.preprocessed[, namesTrain_char] <- NULL

#loading dataset
train <- House.Prices.Kaggle.preprocessed[House.Prices.Kaggle.preprocessed$Id <= 1460,]
test <- House.Prices.Kaggle.preprocessed[House.Prices.Kaggle.preprocessed$Id > 1460,]
test$SalePriceLog <- NULL #This was added in Azure
# trainId <- train$Id
# testId <- test$Id
test <- test %>% select(-Id)
train <- train %>% select(-Id)

train$SalePriceLog <- log(train$SalePriceLog)

#dividing the dataset into train and test
train2<-train[1:1000,]
test2<-train[1001:1460,]

#applying Random Forest
model_rf <- randomForest(SalePriceLog ~ ., data = train2)
preds <- predict(model_rf, select(test2, -SalePriceLog))
#table(preds)
testpreds <- data.frame(cbind(test2, preds))
testpreds$resid <- testpreds$SalePriceLog - testpreds$preds
#ggplot(testpreds, aes(x = resid, y = SalePriceLog)) + geom_point()
rmse(testpreds$SalePriceLog, testpreds$preds)
mse(testpreds$SalePriceLog, testpreds$preds)

# 28729.64 #all

# 28184.29 #top 20 #27859.47
# 28112.33 #top 22 #28571.76 #28336.89

# 29121.25 #top 25
# 29053.73 #top 40

imp <- importance(model_rf)
imp <- imp[sort.list(imp[,1], decreasing = TRUE), ]
imp <- cbind(names(imp), as.data.frame(imp))
names(imp) <- c("col", "imp")

imp2 <- top_n(imp, 18) %>% select(col)
model_rf <- randomForest(SalePriceLog ~ ., data = train2 %>% select(append(as.character(imp2$col), "SalePriceLog")))
preds <- predict(model_rf, select(test2, -SalePriceLog))
testpreds <- data.frame(cbind(test2, preds))
testpreds$resid <- testpreds$SalePriceLog - testpreds$preds
rmse(testpreds$SalePriceLog, testpreds$preds)

House.Prices.Kaggle.preprocessed <- House.Prices.Kaggle.preprocessed %>% select(append(as.character(imp2$col), c("SalePriceLog", "Id")))




#rfeControl is another approach
# control <- rfeControl(functions = rfFuncs,
#                       method = "repeatedcv",
#                       repeats = 3,
#                       verbose = FALSE)
# outcomeName <- 'SalePriceLog'
# predictors<- names(train)[!names(train) %in% outcomeName]
# SalePrice_Pred_Profile <- rfe(train[,predictors], train[,outcomeName], rfeControl = control)
# SalePrice_Pred_Profile







#One hot encoding in R
#dmy <- dummyVars(" ~ .", data = House.Prices.Kaggle.preprocessed, fullRank = T)
dmy <- dummyVars(as.formula(paste0(" ~ ", paste(namesTrain_char, collapse="+"))), data = House.Prices.Kaggle.preprocessed, fullRank = T)
House.Prices.Kaggle.preprocessed <- cbind(House.Prices.Kaggle.preprocessed, data.frame(predict(dmy, newdata = House.Prices.Kaggle.preprocessed)))
House.Prices.Kaggle.preprocessed[, namesTrain_char] <- NULL

#loading dataset
train <- House.Prices.Kaggle.preprocessed[House.Prices.Kaggle.preprocessed$Id <= 1460,]
train <- train %>% select(-Id)
train$SalePriceLog <- log(train$SalePriceLog)






#Boruta
boruta.train <- Boruta(SalePriceLog ~ ., data = train, doTrace = 2)

print(boruta.train)

# results: without one-hot encoding
# Boruta performed 99 iterations in 13.06283 mins.
# 55 attributes confirmed important: BedroomAbvGr, BldgType, BsmtCond.n, BsmtExposure.n, BsmtFinSF1 and 50 more;
# 51 attributes confirmed unimportant: BsmtFinSF2, BsmtFinType2.n, BsmtHalfBath, Condition_Artery, Condition_Feedr and 46
# more;
# 10 tentative attributes left: Alley, Exterior_BrkFace, Exterior_HdBoard, Exterior_MetalSd, HasAlley and 5 more;

# results: with one-hot encoding
# Boruta performed 99 iterations in 19.23001 mins.
# 84 attributes confirmed important: BedroomAbvGr, BldgType.Duplex, BldgType.Twnhs, BldgType.TwnhsE, BsmtCond.n and 79
# more;
# 118 attributes confirmed unimportant: Alley.NONE, Alley.Pave, BldgType.2fmCon, BsmtFinSF2, BsmtHalfBath and 113 more;
# 24 tentative attributes left: BsmtFinType2.n, Exterior_HdBoard, Exterior_MetalSd, Exterior_Plywood, Exterior_Wd.Sdng and
# 19 more;
# Final
# Boruta performed 99 iterations in 19.23001 mins.
# Tentatives roughfixed over the last 99 iterations.
# 90 attributes confirmed important: BedroomAbvGr, BldgType.Duplex, BldgType.Twnhs, BldgType.TwnhsE, BsmtCond.n and 85
# more;
# 136 attributes confirmed unimportant: Alley.NONE, Alley.Pave, BldgType.2fmCon, BsmtFinSF2, BsmtFinType2.n and 131 more;

plot(boruta.train, xlab = "", xaxt = "n")
lz <- lapply(1:ncol(boruta.train$ImpHistory),function(i) boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])

names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)


final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

importantVariables <- as.character(getSelectedAttributes(final.boruta, withTentative = F))
boruta.df <- attStats(final.boruta)
print(boruta.df)

#Select variables + Id and SalePriceLog
House.Prices.Kaggle.preprocessed <- House.Prices.Kaggle.preprocessed %>% select(append(importantVariables, c("SalePriceLog", "Id")))


# Without one-hot encoding
# impVar <- data.frame(append(importantVariables, c("Id", "SalePriceLog")))
# names(impVar) <- "impVar"
# write.csv(impVar, file = "reports//importVarBoruta.csv")
# impVar2 <- read.csv(file = "reports//importVarBoruta.csv")
# impVar2 <- as.character(impVar2$impVar)

# With one-hot encoding
# impVar <- data.frame(append(importantVariables, c("Id", "SalePriceLog")))
# names(impVar) <- "impVar"
# write.csv(impVar, file = "reports//importVarBorutaIndicators.csv")
# impVar2 <- read.csv(file = "reports//importVarBorutaIndicators.csv")
# impVar2 <- as.character(impVar2$impVar)
# House.Prices.Kaggle.preprocessed <- House.Prices.Kaggle.preprocessed[,impVar2]



#For the following steps we need to get rid of the ordered factors
namesTrainFiltered <- colnames(House.Prices.Kaggle.preprocessed)
namesTrainFiltered_fac <- namesTrainFiltered[endsWith(namesTrainFiltered, ".n")]
House.Prices.Kaggle.preprocessed[,namesTrainFiltered_fac] <- lapply(House.Prices.Kaggle.preprocessed[namesTrainFiltered_fac], as.integer)


train <- House.Prices.Kaggle.preprocessed[House.Prices.Kaggle.preprocessed$Id <= 1460,]
test <- House.Prices.Kaggle.preprocessed[House.Prices.Kaggle.preprocessed$Id > 1460,]
test$SalePriceLog <- NULL #This was added in Azure
trainId <- train$Id
testId <- test$Id
test <- test %>% select(-Id)
train <- train %>% select(-Id)
train$SalePriceLog <- log(train$SalePriceLog)

#Spliting training set into two parts based on outcome: 75% and 25%
# index <- createDataPartition(train_transformed$Loan_Status, p=0.75, list=FALSE)
# trainSet <- train_transformed[ index,]
# testSet <- train_transformed[-index,]














# # Add variable that combines above grade living area with basement sq footage
# train$total_sq_footage = train$GrLivArea + train$TotalBsmtSF
# test$total_sq_footage = test$GrLivArea + test$TotalBsmtSF
# 
# # Add variable that combines above ground and basement full and half baths
# train$total_baths = train$BsmtFullBath + train$FullBath + (0.5 * (train$BsmtHalfBath + train$HalfBath))
# test$total_baths = test$BsmtFullBath + test$FullBath + (0.5 * (test$BsmtHalfBath + test$HalfBath))
# 
# # Remove Id since it should have no value in prediction
# train$Id = NULL    
# test$Id = NULL








# Choose the features and classes
xTrain <- train %>% select(-SalePriceLog)
yTrain <- train$SalePriceLog












# custom_summary = function(data, lev = NULL, model = NULL){
#   out = rmsle(data[, "obs"], data[, "pred"])
#   names(out) = c("rmsle")
#   out
# }

fitControl_1 <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = custom_summary
)

fitControl_2 <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  savePredictions = TRUE
  #summaryFunction = custom_summary
)

tuneGrid_1 <- expand.grid(
                   nrounds = c(500), # Test 4 values for boosting rounds
                   max_depth = c(5),           # Test 2 values for tree depth
                   eta = c(0.025),      # Test 3 values for learning rate
                   gamma = c(0), 
                   colsample_bytree = c(0.2), 
                   min_child_weight = c(0.6),
                   subsample = c(1))

tuneGrid_1 <- expand.grid(
  nrounds = c(600, 700), 
  max_depth = c(4),     
  eta = c(0.035),
  gamma = c(0), 
  colsample_bytree = c(0.1), 
  min_child_weight = c(0.7),
  subsample = c(1))

#nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 8     400         5 0.04     0              0.4              0.65         1
# 6     450         5 0.04     0              0.4              0.8         1
# 9     600         5 0.04     0              0.4              0.85         1
# 9     800         5 0.04     0              0.4              1         1
# 1     800         5 0.04     0              0.4              1         1
#29     500         5 0.03     0              0.3              0.8         1
# 2     500         5 0.025    0              0.2              0.6         1
# 6     500         5 0.025    0              0.2              0.6         1

# 333     600         4 0.035     0              0.1              0.7         1

# tuneGrid_2 <- expand.grid(
#   nrounds = c(280, 300, 320, 350), 
#   max_depth = c(3, 4, 5, 6),       
#   eta = c(0.07, 0.06, 0.05, 0.04), 
#   gamma = c(0.1, 0.08, 0.06, 0.04), 
#   colsample_bytree = c(1), 
#   min_child_weight = c(1),
#   subsample = c(1))

#names(getModelInfo())
#modelLookup(model='gbm')
#modelLookup(model='xgbTree')
#modelLookup(model='gbm')

xgbtreeModel <- train(
  x = xTrain, 
  y = yTrain, 
  trControl = fitControl_2, 
  tuneGrid = tuneGrid_1,
  method = "xgbTree",
  metric="RMSE"
#  verbose = 1
)








set.seed(123)

train.test.split <- sample(2
                           , nrow(train)
                           , replace = TRUE
                           , prob = c(0.9, 0.1))
trainTrain = train[train.test.split == 1,]
trainTest = train[train.test.split == 2,]

xTrainTrain <- trainTrain %>% select(-SalePriceLog)
yTrainTrain <- trainTrain$SalePriceLog
xTrainTest <- trainTest %>% select(-SalePriceLog)
yTrainTest <- trainTest$SalePriceLog



xgb.data.trainTrain <- xgb.DMatrix(as.matrix(xTrainTrain), label = yTrainTrain)
xgb.data.trainTest <- xgb.DMatrix(as.matrix(xTrainTest), label = yTrainTest)

xgb.data.train <- xgb.DMatrix(as.matrix(train %>% select(-SalePriceLog)), label = train$SalePriceLog)
xgb.data.test <- xgb.DMatrix(as.matrix(test))
















searchGridSubCol <- expand.grid(
  subsample = c(0.5, 0.65, 0.8, 1.0), 
  colsample_bytree = c(0.6, 0.75, 0.9),
  ntrees = c(500, 1000, 2000),
  gamma = c(0, 0.1, 1),
  eta = c(0.01, 0.05, 0.1),
  min_child_weight = c(1, 10, 30)
)

rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
  
  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]
  currentNTrees <- parameterList[["ntrees"]]
  currentETA <- parameterList[["eta"]]
  currentGamma <- parameterList[["gamma"]]
  currentMin_child_weight <- parameterList[["min_child_weight"]]

  xgboostModelCV <- xgb.cv(data =  xgb.data.train, 
                           nrounds = currentNTrees, 
                           nfold = 8,
                           showsd = FALSE, 
                           metrics = "rmse", 
                           verbose = FALSE, 
                           eval_metric = "rmse",
                           objective = "reg:linear", 
                           max.depth = 6, 
                           gamma = currentGamma,
                           min_child_weight = currentMin_child_weight,
                           eta = currentETA,
                           subsample = currentSubsampleRate, 
                           colsample_bytree = currentColsampleRate
                             , early_stopping_rounds = 200
                            # , print_every_n = 200
                           )
  rmsete <- tail(xgboostModelCV$evaluation_log$test_rmse_mean, 1)
  rmsetr <- tail(xgboostModelCV$evaluation_log$train_rmse_mean, 1)
  return(c(rmsete, rmsetr, currentSubsampleRate, currentColsampleRate, currentNTrees, currentETA, currentMin_child_weight, currentGamma))
})

rmseErrorsHyperparameters


xgbresults <- rmseErrorsHyperparameters


xgbresults.df <- as.data.frame(unlist(t(xgbresults)))
names(xgbresults.df) <- c("rmsete", "rmsetr", "SubsampleRate", "ColsampleRate", "nTrees", "ETA", "Min_child_weight", "Gamma")

xgbresults.df <- xgbresults.df %>% 
  filter(
    rmsete < 0.14, 
    rmsetr > 0.09
  )

xgbresults.df.order <- xgbresults.df %>% 
    arrange(
      round(rmsete, digits = 3), 
      desc(rmsetr - rmsete)
    )
gg <- ggplot(xgbresults.df, aes(rmsete, rmsetr, nTrees, ETA, Min_child_weight, Gamma, SubsampleRate, ColsampleRate))
gg + geom_point(aes(alpha = 0.3, colour = factor(ETA), shape = factor(Gamma)))
gg + geom_point(aes(alpha = 0.3, colour = factor(ETA), shape = factor(SubsampleRate)))
gg + geom_point(aes(alpha = 0.3, colour = factor(ETA), shape = factor(ColsampleRate)))
gg + geom_point(aes(alpha = 0.3, colour = factor(ETA), shape = factor(nTrees)))
gg + geom_point(aes(alpha = 0.3, colour = factor(ETA), shape = factor(Min_child_weight)))
                                                                                       
#ETA 0.01
#subsample rate 0.65
#ntrees 1500
#ColsampleRate 0.75
#gamma 0.1
#Min_child_weight 30


#Now lets create the model with these fine tuned params
#xgb.create.features()
xgb.model.cv <- xgb.cv(
                        data =  xgb.data.train, 
                        params = list(
                           objective = "reg:linear" #default
                         , eta = 0.01
                         #, gamma = 0.1
                         , max.depth = 3
                         , min_child_weight = 20
                         , subsample = 0.9
                         , colsample_bytree = 0.8
                         #, num_parallel_tree = 1
                         #, nthread = 2
                         , eval_metric = "rmse"
                         # , tree_method = "hist"
                         #  , grow_policy = "lossguide"
                       )
                       #, watchlist = list(test = xgb.data.test)
                       , nrounds = 10000
                       , nfold = 8
                       #, early_stopping_rounds = 500
                       , print_every_n = 1000
                       , verbose = 1
)


xgb.model <- xgb.train(data = xgb.data.train
                            , params = list(
                                objective = "reg:linear" #default
                              , eta = 0.01
                              #, gamma = 0.1
                              , max.depth = 3
                              , min_child_weight = 20
                              , subsample = 0.9
                              , colsample_bytree = 0.8
                              #, num_parallel_tree = 1
                              #, nthread = 2
                              , eval_metric = "rmse"
                              # , tree_method = "hist"
                              #  , grow_policy = "lossguide"
                            )
                            #, watchlist = list(test = xgb.data.test)
                            , nrounds = 10000
                            #, early_stopping_rounds = 500
                            , print_every_n = 500
                            , verbose = 1
)
print(xgb.model)
xgb.model
xgb.importance(model = xgb.model)

xgb.data.train.results <- predict(xgb.model
                                 , newdata = xgb.data.train)
library(ModelMetrics)
#rmse(actual = train$SalePriceLog, predicted = train$SalePriceLog)
rmse(train$SalePriceLog, xgb.data.train.results)

xgb.data.train.results <- exp(xgb.data.train.results)


#new.features.train <- xgb.create.features(model = xgb.model, xgb.data.train)
#Geen zin om deze fout nog aan te passen
#Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
# contrasts can be applied only to factors with 2 or more levels
#new.xgb.data.train <- xgb.DMatrix(data = new.features.train, label = new.features.train$SalePriceLog)


xgb.data.test.results <- predict(xgb.model
                        , newdata = xgb.data.test)

xgb.data.test.results <- exp(xgb.data.test.results)



print(xgb.data.test.results)


testResults <- test
testResults <- cbind(testResults, salePriceXGB)
testResults <- cbind(testResults, xgb.data.test.results)
#results <- data.frame(Id = testId, SalePrice = xgb.data.test.results)
#write.csv(results, file = "reports//Submission.csv", row.names=FALSE)

ggplot(testResults, aes(salePriceXGB, xgb.data.test.results)) + geom_point() 







# xgbtreeTrain <- train(
#   x = xTrain, 
#   y = yTrain, 
#   trControl = fitControl_2, 
#   tuneLength = 10,
#   method = "xgbTree",
#   metric="rmsle",
#   maximize = FALSE,
#   verbose = 1
# )

# print(xgbtreeModel)
# plot(xgbtreeModel)
# xgbtreeModel$results
# xgbtreeModel$bestTune
# varImp(xgbtreeModel)
# 
# xgbtreeModel


# xgbTrainPred <- xgbtreeModel$pred
# xgbTrainPredSelected <- xgbTrainPred %>% filter(
#   nrounds == 400 & subsample == 1 & eta == 0.04 & max_depth == 5 & gamma == 0 & colsample_bytree == 0.4 & min_child_weight == 0.6
# )

xgbTrainPred <- data.frame(predict(xgbtreeModel, xTrain))
names(xgbTrainPred) <- "xgbTrainPred"
train <- cbind (train, xgbTrainPred)

train$xgbResidual <- train$SalePriceLog - train$xgbTrainPred

varImp(object=xgbtreeModel)



#ggplot(train, aes(SalePriceLog, xgbTrainPred, xgbResidual)) + geom_r



xgbTestPred <- predict(xgbtreeModel, newdata=test)
salePriceXGB <- exp(xgbTestPred)
salePriceXGB <- xgbTestPred

testResults <- test
testResults <- cbind(testResults, salePriceXGB)
#results <- data.frame(Id = testId, SalePrice = salePriceXGB)
#write.csv(results, file = "reports//Submission.csv", row.names=FALSE)





library(xgboost)

#install.packages("lightgbm")


# library(devtools)
# devtools::install_github("Microsoft/LightGBM", subdir = "R-package")

library(R6)
library(Matrix)
library(lightgbm)
library(data.table)
install.packages("data.table")

# data(agaricus.train, package='lightgbm')
# trainn <- agaricus.train
# dtrainn <- lgb.Dataset(trainn$data, label=trainn$label)
# params <- list(objective="regression", metric="l2")
# model <- lgb.cv(params, dtrainn, 10, nfold=5, min_data=1, learning_rate=1, early_stopping_rounds=10)
# rm("trainn","dtrainn","params","model","agaricus.train")


#xgb.data.train <- xgb.DMatrix(as.matrix(xTrain[, colnames(xTrain) != "Class"]), label = yTrain)

lgb.train = lgb.Dataset(as.matrix(xTrain[, colnames(xTrain) != "Class"]), label = yTrain)
#lgb.test = lgb.Dataset(as.matrix(test[, colnames(test) != "Class"]), label = test$Class)

#xgb.data.test <- xgb.DMatrix(as.matrix(test[, colnames(test) != "Class"]), label = test$Class)

#train_lgb <- lgb.Dataset(xTrain,label=yTrain)

# 
# learning_rate = 0.1
# num_leaves = 255
# num_trees = 500
# num_threads = 16
# min_data_in_leaf = 0
# min_sum_hessian_in_leaf = 100

params.lgb = list(
  objective = "regression"
  , metric = "rmse"
  , feature_fraction = 1
  , bagging_fraction = 1
  , bagging_freq = 0
)

lgb.model <- lgb.train(
    params = params.lgb
    , data = lgb.train
    #, valids = list(test = lgb.test)
    , learning_rate = 0.1
    , num_leaves = 10
    , num_trees = 500
    , num_threads = 2
    , nrounds = 500
    #, min_data_in_leaf = 0
    , min_sum_hessian_in_leaf = 100
    #, early_stopping_rounds = 40
    #, eval_freq = 50
  )



print(max(unlist(lgb.model$record_evals[["test"]][["auc"]][["eval"]])))

# get feature importance
lgb.feature.imp = lgb.importance(lgb.model, percentage = TRUE)

# make test predictions
lgb.test = predict(lgb.model, data = as.matrix(test[, colnames(test) != "Class"]), n = lgb.model$best_iter)
auc.lgb = roc(test$Class, lgb.test, plot = TRUE, col = "green")
print(auc.lgb)





##Stochastic Gradient Boosting
#trainGBM <- xTrain #train %>% select(-Id, -xgbTrainPred)

fitControl_1GBM <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
#tuneGrid_1GBM <- expand.grid(n.trees = 5000, interaction.depth = 4:6, shrinkage = 0.006 + 0.001 * 1 : 3, n.minobsinnode = c(5, 10, 15))
tuneGrid_1GBM <- expand.grid(n.trees = 1000, interaction.depth = 4, shrinkage = 0.006, n.minobsinnode = c(10))

gbmModel  <- train(
  x = xTrain, 
  y = yTrain,  
  method = "gbm", 
  distribution = "gaussian", 
  tuneGrid = tuneGrid_1GBM, 
  trControl = fitControl_1GBM
)


gbmTrainPred <- data.frame(predict(gbmModel, xTrain))
names(gbmTrainPred) <- "gbmTrainPred"
train <- cbind (train, gbmTrainPred)

test_predictionsGBM <- predict(gbmModel, newdata=test)
#salePriceGBM <- exp(test_predictionsGBM)
salePriceGBM <- test_predictionsGBM

testResults <- cbind(testResults, salePriceGBM)




##ElasticNet
#    alpha lambda
# 21  0.15  0.015
#400  0.03   0.03
#400  0.04   0.04
#202  0.051  0.042  
#331  0.042  0.031
fitControl_1EN <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
#tuneGrid_1EN <- expand.grid(alpha = seq(0, 0.5, 0.01), lambda = 0.01 * 1:5))
tuneGrid_1EN <- expand.grid(alpha = 0.03 + 0.001 * 1:30, lambda = 0.03 + 0.001 * 1:30)
ElasticNetModel  <- train(x = xTrain, 
                          y = yTrain, 
                        method = "glmnet", family = "gaussian", 
                        trControl = fitControl_1EN, tuneGrid = tuneGrid_1EN)

ElasticNetModel$bestTune

ElasticNetTrainPred <- data.frame(predict(ElasticNetModel, xTrain))
names(ElasticNetTrainPred) <- "ElasticNetTrainPred"
train <- cbind (train, ElasticNetTrainPred)


#Predictions ElasticNet
test_predictionsEN <- predict(ElasticNetModel, newdata = test)
#salePriceEN <- exp(test_predictionsEN)
salePriceEN <- test_predictionsEN

testResults <- cbind(testResults, salePriceEN)


#??????????????????? MODEL as a fourth


trainResults <- train
trainResults <- trainResults %>%
  mutate(
    SalePriceLog = exp(SalePriceLog),
    xgbTrainPred = exp(xgbTrainPred),
    gbmTrainPred = exp(gbmTrainPred),
    ElasticNetTrainPred = exp(ElasticNetTrainPred)
  )

trainResults <- trainResults %>%
  mutate(
    xgbResidial = xgbTrainPred - SalePriceLog,
    gbmResidial = gbmTrainPred - SalePriceLog,
    ElasticNetResidial = ElasticNetTrainPred - SalePriceLog
  )


RMSE(trainResults$xgbTrainPred, trainResults$SalePriceLog)
RMSE(trainResults$gbmTrainPred, trainResults$SalePriceLog)
RMSE(trainResults$ElasticNetTrainPred, trainResults$SalePriceLog)

trainResults2 <- trainResults %>%
  select(
    c(SalePriceLog, xgbTrainPred, gbmTrainPred, ElasticNetTrainPred)
  )

plot(trainResults2)

trainResults3 <- trainResults2 %>% mutate (
    PredSalePriceLog = 
      ElasticNetTrainPred * 0.20 +
      gbmTrainPred * 0.2 +
      xgbTrainPred * 0.6
  )

RMSE(trainResults3$SalePriceLog, trainResults3$PredSalePriceLog)


testResults <- testResults %>% mutate (
  SalePrice = exp(
    salePriceEN * 0.15 +
    salePriceGBM * 0.15 +
    salePriceXGB * 0.7
  )
)


results <- data.frame(Id = testId, SalePrice = testResults$SalePrice)
write.csv(results, file = "Submission.csv", row.names=FALSE)





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