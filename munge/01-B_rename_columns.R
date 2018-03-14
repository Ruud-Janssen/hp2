#rename variables .n means in this case it is a factor
House.Prices.Kaggle.preprocessed$OverallCond.n <- House.Prices.Kaggle.preprocessed$OverallCond
House.Prices.Kaggle.preprocessed$OverallCond <- NULL
House.Prices.Kaggle.preprocessed$OverallQual.n <- House.Prices.Kaggle.preprocessed$OverallQual
House.Prices.Kaggle.preprocessed$OverallQual <- NULL

# Renaming variables because ranger can not handle column names starting with numbers
House.Prices.Kaggle.preprocessed$FirstFlrSf    <- House.Prices.Kaggle.preprocessed$`1stFlrSF`
House.Prices.Kaggle.preprocessed$`1stFlrSF`    <- NULL
House.Prices.Kaggle.preprocessed$SecondFlrSf   <- House.Prices.Kaggle.preprocessed$`2ndFlrSF`
House.Prices.Kaggle.preprocessed$`2ndFlrSF`    <- NULL
House.Prices.Kaggle.preprocessed$ThirdSsnPorch <- House.Prices.Kaggle.preprocessed$`3SsnPorch`
House.Prices.Kaggle.preprocessed$`3SsnPorch`   <- NULL