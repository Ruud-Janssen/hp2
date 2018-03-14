library(ProjectTemplate)
load.project()

clear.cache()
rm(list = ls() )

getwd()


test_dir('diagnostics/')
test_file("diagnostics//test_diagnostic_preprocessed_data.R")

test.project() 



#Notes problems with parallel processing packages
#install.packages("multidplyr")
#install.packages("doMC") 

#install.packages("gbm3") #not available for 3.4 R....

#reshape, plyr, dplyr, ggplot2, stringr, lubridate, data.table, testthat, tidyverse, glmnet, ICC, ranger, C50, mlbench, gridExtra, parallel, caret, corrplot, GGally, e1071, xgboost, Metrics, ggthemes, randomForest, Boruta
install.packages("caret") 
library(caret)
library(ggplot2)
library(xgboost)
