library(tidyverse)
library(glmnet)
library(gbm3)
library(caret)
library(C50)
library(mlbench)

train <- read_csv("input/House Prices Kaggle_preprocessed.csv")


fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 1, returnResamp="all")

# Choose the features and classes
x <- as.data.frame(train %>% select(-SalePriceLog))
y <- train$SalePriceLog

#grid <- expand.grid(.mstop = c(5000, 10000, 15000), .maxdepth=c(10,15,20), .nu=c(0.001, 0.002, 0.005))
grid <- expand.grid(.mstop = c(5000, 10000), .maxdepth=c(10), .nu=c(0.001))

mdl<- train(x = x,y = y,tuneGrid = grid, trControl = fitControl, method = "bstTree")

mdl

# visualize the resample distributions
xyplot(mdl,type = c("g", "p", "smooth"))






# f = SalePriceLog ~ .
# 
# train_params <- training_params(num_trees = 200000,
#                                 shrinkage = 0.0175,
#                                 bag_fraction = 0.5,
#                                 num_train = round(nrow(train) / 2),
#                                 id=seq_len(nrow(train)),
#                                 min_num_obs_in_node = 10,
#                                 interaction_depth = 3,
#                                 num_features = 2)
# 
# gbm1 = gbmt(formula = f, data = train, distribution= gbm_dist("Gaussian"), train_params = train_params, 
#             keep_gbm_data = TRUE)
# plot(gbm1$valid.error)
# min(gbm1$valid.error)



