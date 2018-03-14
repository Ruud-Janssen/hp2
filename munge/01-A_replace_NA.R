House.Prices.Kaggle.preprocessed <- House.Prices.Kaggle.preprocessed %>%
  mutate(
    BsmtFullBath = ifelse(BsmtFullBath == "NONE", 0, BsmtFullBath),
    BsmtHalfBath = ifelse(BsmtHalfBath == "NONE", 0, BsmtHalfBath)
  )
