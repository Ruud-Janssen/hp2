# #Does the data contain missing values
# rm("data")
# data <- NULL
# sum(is.na(data))
# 
# 
# #Imputing missing values using median
# preProcValues <- preProcess(data, method = c("medianImpute","center","scale"))
# library('RANN')
# data_processed <- predict(preProcValues, data)
# 
# sum(is.na(data_processed))
