#Adapt some default datatypes after the import
House.Prices.Kaggle.preprocessed$MSSubClass <- as.character(House.Prices.Kaggle.preprocessed$MSSubClass)
House.Prices.Kaggle.preprocessed$BsmtFullBath <- as.integer(House.Prices.Kaggle.preprocessed$BsmtFullBath)
House.Prices.Kaggle.preprocessed$BsmtHalfBath <- as.integer(House.Prices.Kaggle.preprocessed$BsmtHalfBath)