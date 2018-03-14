House.Prices.Kaggle.preprocessed$SalePriceLog <- exp(House.Prices.Kaggle.preprocessed$SalePriceLog)

#train_hp_preprocessed <- House.Prices.Kaggle.preprocessed[House.Prices.Kaggle.preprocessed$Id <= 1460,]
#test_hp_preprocessed <- House.Prices.Kaggle.preprocessed[House.Prices.Kaggle.preprocessed$Id > 1460,]
#test_hp_preprocessed$SalePriceLog <- NULL #This was added in Azure
