##Starting out with R - House prices predictions

library(ggplot2)
library(survival)
library(plyr)
library(dplyr)
library(stringr)
library(data.table)
library(tidyr)
library(corrplot)
library(Metrics)
library(caret)
library(DT)  #Data table- datatable()
library(dummies)
library(corrplot)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)
library(glmnet)
library(gbm)
library(Matrix)
library(iterators)
library(parallel)
library(xgboost)
library(parallel)
library(parallelMap) 
library(caretEnsemble)
library(ensembleR)
library(caTools)
library(mlbench)
library(party)
library(ranger)
library(lars)
library(kknn)

train<- read.csv("../input/train.csv",header = T,stringsAsFactors = F)
test <- read.csv("../input/test.csv",header = T,stringsAsFactors = F)


##1.data cleaning & preparation:
#(a)tackling outliers-
ggplot(train,aes(y=SalePrice,x=GrLivArea))+ggtitle("With Outliers")+geom_point()
train[train$GrLivArea>4000&train$SalePrice<2e+05,]$GrLivArea <- mean(train$GrLivArea)%>%as.numeric
ggplot(train,aes(y=SalePrice,x=GrLivArea))+ggtitle("Without Outliers")+geom_point()

#(b)Check the distribution of Sale price (normal distribution?)& combine datasets-
ggplot(train,aes(SalePrice))+geom_histogram(fill="steelblue",color="black")
ggplot(train,aes(SalePrice))+geom_histogram(fill="steelblue",color="black")+scale_x_log10()
train$SalePrice <- log(train$SalePrice+1)
test$SalePrice <- as.numeric(0)
combi <- rbind(train,test)

#(c)Handling Missing values-
missing_values <- train %>% summarise_all(funs(sum(is.na(.)/n())))
missing_values
missing_values <- gather(missing_values,key = "feature",value = "missing_pct")
missing_values
ggplot(missing_values,aes(x=feature,y=missing_pct))+geom_bar(stat="identity",fill="blue")+
  coord_flip()+theme_bw()
#nas_filtered <- filter(missing_values, missing_pct<0.8)
#Before that, take a deep look at the dataset first. The missing values here are not all the common NAs.
#The majority of them represent that no such kind of thing! 
#So we have to transform these NAs instead of deleting them or replacing them with mean.
combi$LotFrontage[is.na(combi$LotFrontage)] <- 0
combi$MasVnrArea[is.na(combi$MasVnrArea)] <- 0
combi$BsmtFinSF1[is.na(combi$BsmtFinSF1)] <- 0
combi$BsmtFinSF2[is.na(combi$BsmtFinSF2)] <- 0
combi$BsmtUnfSF[is.na(combi$BsmtUnfSF)] <- 0
combi$TotalBsmtSF[is.na(combi$TotalBsmtSF)] <- 0
combi$BsmtFullBath[is.na(combi$BsmtFullBath)] <- 0
combi$BsmtHalfBath[is.na(combi$BsmtHalfBath)] <- 0
combi$GarageYrBlt[is.na(combi$GarageYrBlt)] <- 0
combi$GarageCars[is.na(combi$GarageCars)] <- 0
combi$GarageArea[is.na(combi$GarageArea)] <- 0
#Also notice that there are some obvious typos in the dataset.
combi$GarageYrBlt[combi$GarageYrBlt==2207] <- 2007
combi[is.na(combi)] <- "None"

#(d)Recode ordered factors as pseudo-continuous numerical variables -
combi$ExterQual<- recode(combi$ExterQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$ExterCond<- recode(combi$ExterCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$BsmtQual<- recode(combi$BsmtQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$BsmtCond<- recode(combi$BsmtCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$BsmtExposure<- recode(combi$BsmtExposure,"None"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=4)
combi$BsmtFinType1<- recode(combi$BsmtFinType1,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
combi$BsmtFinType2<- recode(combi$BsmtFinType2,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
combi$HeatingQC<- recode(combi$HeatingQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$KitchenQual<- recode(combi$KitchenQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$Functional<- recode(combi$Functional,"None"=0,"Sev"=1,"Maj2"=2,"Maj1"=3,"Mod"=4,"Min2"=5,"Min1"=6,"Typ"=7)
combi$FireplaceQu<- recode(combi$FireplaceQu,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$GarageFinish<- recode(combi$GarageFinish,"None"=0,"Unf"=1,"RFn"=2,"Fin"=3)
combi$GarageQual<- recode(combi$GarageQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$GarageCond<- recode(combi$GarageCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$PoolQC<- recode(combi$PoolQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$Fence<- recode(combi$Fence,"None"=0,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=4)
#Adding an important feature - Total area of basement
combi$TotalSF = combi$TotalBsmtSF + combi$X1stFlrSF + combi$X2ndFlrSF

#(e)Creating dummies & rename some columns -
combi_dummy <-dummy.data.frame(combi,dummy.classes = "character")
combi_dummy <- rename(combi_dummy,"MSZoningC"="MSZoningC (all)")
combi_dummy <- rename(combi_dummy,"RoofMatlTarGrv"="RoofMatlTar&Grv")
combi_dummy <- rename(combi_dummy,"Exterior1stWdSdng"="Exterior1stWd Sdng")
combi_dummy <- rename(combi_dummy,"Exterior2ndBrkCmn"="Exterior2ndBrk Cmn")
combi_dummy <- rename(combi_dummy,"Exterior2ndWdSdng"="Exterior2ndWd Sdng")
combi_dummy <- rename(combi_dummy,"Exterior2ndWdShng"="Exterior2ndWd Shng")

#(f)Transform those values with high skewness-
#(1)log+1 transform(Thanks for siddharth raina's "Regularized Regression - Housing Pricing")-
#combi2 <- combi_dummy[,-241]
#Col_class<- sapply(names(combi2),function(x){class(combi2[[x]])})
#col_num <- names(Col_class[Col_class != "character"])
#determining skew of each numric variable
#skew <- sapply(col_num,function(x){skewness(combi2[[x]],na.rm = T)})
# Let us determine a threshold skewness and transform all variables above the treshold.
#skew <- skew[skew > 0.75]
# transform excessively skewed features with log(x + 1)
#for(x in names(skew)) {
#  combi_dummy[[x]] <- log(combi_dummy[[x]] + 1)
#}

#OR
#(2)BoxCoxTransform(recommended)-
feature_classes <- sapply(names(combi_dummy), function(x) {
  class(combi_dummy[[x]])
})
numeric_feats <- names(feature_classes[feature_classes != "character"])
skewed_feats <- sapply(numeric_feats, function(x) {
  skewness(combi_dummy[[x]], na.rm = TRUE)
})
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.75]
for (x in names(skewed_feats)) {
  bc = BoxCoxTrans(combi_dummy[[x]], lambda = 0.15)
  combi_dummy[[x]] = predict(bc, combi_dummy[[x]])
}

#(g)Split combined data back into test,train and validation dataframes
train_dummy <- combi_dummy[1:1460,]
test_dummy <- combi_dummy[1461:2919,]
set.seed(123)
in_train <- createDataPartition(train_dummy$SalePrice,p=0.7,list=F)
new_train <- train_dummy[in_train,]
validation <- train_dummy[-in_train,]


##2.Model building and evaluation
##(a).MLR
#First include all predictor variables to see what will happen
mlr <-lm(formula = SalePrice ~., data = new_train) 
getOption("max.print")
options(max.print = 2000)
summary(mlr)
prediction<- predict(mlr,validation, type="response")
rmse(validation$SalePrice,prediction)
#[1] 0.1456502
#kaggle score 0.13431.

##(b).Decision Trees
myformula <- SalePrice~.
modfit <- train(myformula,method="rpart",data=new_train)
prediction_2 <- predict(modfit,newdata =validation)
rmse(validation$SalePrice,prediction_2)
#[1] 0.2830219
##kaggle score 0.29350

##(c).Random Forest
rf <- randomForest(SalePrice~.,data=new_train,ntree=1000,proximity=TRUE)
varImpPlot(rf)
prediction_3 <- predict(rf,newdata =validation)
rmse(validation$SalePrice,prediction_3)
#[1] 0.1400461
#kaggle score 0.14067

##(d).Regularized Regression(Lasso)
all_predictors <- subset(train,select = -c(SalePrice))
var_classes <- sapply(all_predictors,function(x)class(x))
num_classes <- var_classes[var_classes!="character"]
num_vars <- subset(train,select=names(num_classes))
#corrplot(cor(num_vars),method="number")
corrplot(cor(num_vars),method="circle")
#Building model
set.seed(123)
lasso <-cv.glmnet(as.matrix(new_train[, -241]), new_train[, 241])
prediction_4 <- predict(lasso, newx = as.matrix(validation[, -241]), s = "lambda.min")
rmse(validation$SalePrice,prediction_4)
#[1] 0.1270746
#kaggle score 0.12573

##(e).Gradient Boosting model(GBM)
set.seed(1)
cv.ctrl_gbm <- trainControl(method="repeatedcv",number=5,repeats = 5)
gbm<- train(SalePrice ~ ., method = "gbm", metric = "RMSE", maximize = FALSE, 
            trControl =cv.ctrl_gbm, tuneGrid = expand.grid(n.trees = 700, 
                                                           interaction.depth = 5, shrinkage = 0.05,
                                                           n.minobsinnode = 10), data = new_train,verbose = FALSE)
varImp(gbm)
prediction_5 <- predict(gbm,newdata = validation)
rmse(validation$SalePrice,prediction_5)
#[1] 0.1288874
#kaggle score 0.12457

##(f).XGBOOST(Extreme Gradient Boosting) 
#preparing matrix 
dtrain <- xgb.DMatrix(data = as.matrix(new_train[,-241]),label = as.matrix(new_train$SalePrice)) 
dtest <- xgb.DMatrix(data = as.matrix(validation[,-241]),label=as.matrix(validation$SalePrice))
#Building model
set.seed(111)
xgb <-  xgboost(booster="gbtree",data = dtrain, nfold = 5,nrounds = 2500, verbose = FALSE, 
                objective = "reg:linear", eval_metric = "rmse", nthread = 8, eta = 0.01, 
                gamma = 0.0468, max_depth = 6, min_child_weight = 1.41, subsample = 0.769, colsample_bytree =0.283)
mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb)
xgb.plot.importance (importance_matrix = mat[1:20]) 
prediction_6 <- predict(xgb,newdata = dtest)
rmse(validation$SalePrice,prediction_6)
#[1] 0.1210158
#kaggle score 0.12623

##(g)Simple Average RMSE of Lasso+GBM+XGBoost(Top3 performance models)
rmse(validation$SalePrice, (prediction_4 + prediction_5 + prediction_6)/3)
#[1] 0.1184966473

##(h)Weighted Average RMSE of Lasso+GBM+XGBoost
rmse(validation$SalePrice, (0.3 *prediction_4 + 0.1 *prediction_5 + 0.6 *prediction_6))
#[1] 0.1185069295

##(i)Ensemble method
my_control <- trainControl(method="boot",number=5,savePredictions="final")
set.seed(11)
model_list <- caretList(
  SalePrice ~ ., data=new_train,
  trControl=my_control,
  metric="RMSE",
  methodList=c("knn","glmnet"),
  tuneList=list(
    gbm=caretModelSpec(method="gbm", tuneGrid=expand.grid(n.trees = 700, interaction.depth = 5, 
                                                           shrinkage = 0.05,n.minobsinnode = 10)),
    xgbTree=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = 2500,max_depth = 6,min_child_weight=1.41,
                                                               eta =0.01,gamma = 0.0468,subsample=0.769,
                                                               colsample_bytree =0.283))
  )
)
modelCor(resamples(model_list))

##Simple Blending
set.seed(123456)
greedy_ensemble <- caretEnsemble(model_list, metric="RMSE",trControl=trainControl(number=25))
greedy_ensemble
varImp(greedy_ensemble)
summary(greedy_ensemble)
prediction_7 <- predict(greedy_ensemble,newdata = validation)
rmse(validation$SalePrice,prediction_7)
#3:[1] 0.1196111594 

# Using a “meta-model”
set.seed(1)
rf_ensemble <- caretStack(model_list,method="rf",metric="RMSE",
  trControl=trainControl(method="boot",number=5,savePredictions="final"))
prediction_8 <- predict(rf_ensemble,newdata = validation)
rmse(validation$SalePrice,prediction_8)
#3:[1] 0.1249261368 

##3.Retraining on whole training set and Final Submission
#Lasso
set.seed(123)
lasso <-  cv.glmnet(as.matrix(train_dummy[, -241]), train_dummy[, 241])
pred_1 <- as.numeric(exp(predict(lasso, newx = as.matrix(test_dummy[, -241]), s = "lambda.min"))-1)
#GBM
set.seed(1)
gbm_1<- train(SalePrice ~ ., method = "gbm", metric = "RMSE", maximize = FALSE, 
            trControl =cv.ctrl_gbm, tuneGrid = expand.grid(n.trees = 700, 
                                                           interaction.depth = 5, shrinkage = 0.05,
                                                           n.minobsinnode = 10), data = train_dummy,verbose = FALSE)
pred_2 <- exp(predict(gbm_1,newdata = test_dummy))-1
#XGBoost
set.seed(111)
dtrain_1 <- xgb.DMatrix(data = as.matrix(train_dummy[,-241]),label = as.matrix(train_dummy$SalePrice)) 
dtest_1 <- xgb.DMatrix(data = as.matrix(test_dummy[,-241]),label=as.matrix(test_dummy$SalePrice))
xgb_1 <-  xgboost(booster="gbtree",data = dtrain_1, nfold = 5,nrounds = 2500, verbose = FALSE, 
                  objective = "reg:linear", eval_metric = "rmse", nthread = 8, eta = 0.01, 
                  gamma = 0.0468, max_depth = 6, min_child_weight = 1.41, subsample = 0.769, colsample_bytree =0.283)
pred_3 <- exp(predict(xgb_1, newdata = dtest_1)) - 1
#Simple Average RMSE
submission_1 <- data.frame(Id = test$Id, SalePrice= (pred_1 +pred_2 + pred_3)/3)
write.csv(submission_1, "submission_1.csv", row.names = FALSE) #Kaggle score 0.12036
#Weighted Average RMSE
submission_2 <- data.frame(Id = test$Id, SalePrice= 0.3*pred_1 +0.1*pred_2 + 0.6*pred_3)
write.csv(submission_2, "submission_2.csv", row.names = FALSE) #Kaggle score 0.12077

#Ensemble method
set.seed(11)
model_list <- caretList(
  SalePrice ~ ., data=train_dummy,
  trControl=my_control,
  metric="RMSE",
  methodList=c("knn","glmnet"),
  tuneList=list(
    gbm=caretModelSpec(method="gbm", tuneGrid=expand.grid(n.trees = 700, interaction.depth = 5, 
                                                          shrinkage = 0.05,n.minobsinnode = 10)),
    xgbTree=caretModelSpec(method="xgbTree", tuneGrid=expand.grid(nrounds = 2500,max_depth = 6,min_child_weight=1.41,
                                                                  eta =0.01,gamma = 0.0468,subsample=0.769,
                                                                  colsample_bytree =0.283))
  )
)
###Simple Blending
set.seed(123456)
greedy_ensemble <- caretEnsemble(model_list, metric="RMSE",trControl=trainControl(number=25))
pred_4 <- exp(predict(greedy_ensemble,newdata = test_dummy))-1
submission_3 <- data.frame(Id = test$Id, SalePrice= pred_4)
write.csv(submission_3, "submission_3.csv", row.names = FALSE) #Kaggle score 0.12089
# Using a “meta-models”
set.seed(1)
rf_ensemble <- caretStack(model_list,method="rf",metric="RMSE",
                          trControl=trainControl(method="boot",number=5,savePredictions="final"))
pred_5 <- exp(predict(rf_ensemble,newdata = test_dummy))-1
submission_4 <- data.frame(Id = test$Id, SalePrice= pred_5)
write.csv(submission_4, "submission_4.csv", row.names = FALSE) #Kaggle score 0.12776