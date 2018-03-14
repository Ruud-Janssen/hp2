namesTrain   <- names(House.Prices.Kaggle.preprocessed)

#indicator columns
#train_ind <- House.Prices.Kaggle.preprocessed[, sapply(House.Prices.Kaggle.preprocessed, function(vec) length(unique(vec)) == 2 & ifelse(class(vec) == "integer", sum(unique(vec)), 0) == 1)]
namesTrain_ind <- colnames(House.Prices.Kaggle.preprocessed[, sapply(House.Prices.Kaggle.preprocessed, function(vec) length(unique(vec)) == 2 & ifelse(class(vec) == "integer", sum(unique(vec)), 0) == 1)])

namesTrain_int   <- namesTrain[sapply(House.Prices.Kaggle.preprocessed, is.integer)]
namesTrain_fac <- namesTrain[endsWith(namesTrain, ".n")]
namesTrain_num   <- namesTrain[sapply(House.Prices.Kaggle.preprocessed, is.numeric)]
namesTrain_char   <- namesTrain[sapply(House.Prices.Kaggle.preprocessed, is.character)]

#not waterproof year...
#train_year <- House.Prices.Kaggle.preprocessed[, sapply(House.Prices.Kaggle.preprocessed, function(vec) ifelse(class(vec) == "integer", between(mean(unique(vec)), 1900, 2100), F) == T)]
namesTrain_date <- colnames(House.Prices.Kaggle.preprocessed[, sapply(House.Prices.Kaggle.preprocessed, function(vec) ifelse(class(vec) == "integer", between(mean(unique(vec)), 1900, 2100), F) == T)])
namesTrain_date <- append(namesTrain_date, c("YrMoSold", "MoSold")) #"YrMoSoldCount"

namesTrain_id <- c("Id")

#now remove some overlap
namesTrain_num <- setdiff(namesTrain_num, namesTrain_id)
namesTrain_int <- setdiff(namesTrain_int, namesTrain_id)
namesTrain_num <- setdiff(namesTrain_num, namesTrain_date)
namesTrain_int <- setdiff(namesTrain_int, namesTrain_date)
namesTrain_int <- setdiff(namesTrain_int, namesTrain_fac)
namesTrain_num <- setdiff(namesTrain_num, namesTrain_fac)
namesTrain_int <- setdiff(namesTrain_int, namesTrain_ind)

namesTrain_num <- setdiff(namesTrain_num, namesTrain_int)
namesTrain_num <- setdiff(namesTrain_num, namesTrain_ind)



