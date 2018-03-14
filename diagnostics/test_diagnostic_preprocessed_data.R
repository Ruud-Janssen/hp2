context('diagnostic test preprocessed data')

test_that('data dimensions correct', {
  expect_equal(ncol(House.Prices.Kaggle.preprocessed), 118)
  expect_equal(nrow(House.Prices.Kaggle.preprocessed), 2919)
})

test_that('no missing values', {
  expect_identical(House.Prices.Kaggle.preprocessed, na.omit(House.Prices.Kaggle.preprocessed))
})

test_that('data types correct', {
  expect_is(House.Prices.Kaggle.preprocessed,'data.frame')
  expect_is(House.Prices.Kaggle.preprocessed$MSSubClass, 'character')
})

test_that('data types categorization complete', {
  expect_equal(length(setdiff(setdiff(setdiff(setdiff(setdiff(setdiff(setdiff(namesTrain, namesTrain_char), namesTrain_date), namesTrain_fac), namesTrain_id), namesTrain_ind), namesTrain_int), namesTrain_num)), 0)
  expect_equal(length(namesTrain), length(namesTrain_char) + length(namesTrain_date) + length(namesTrain_fac) + length(namesTrain_id) + length(namesTrain_ind) + length(namesTrain_int) + length(namesTrain_num))
})

# If You need NA count of all —
# table(is.na(House.Prices.Kaggle.preprocessed))
# sapply(House.Prices.Kaggle.preprocessed, function(x) sum(is.na(x)))
# rowSums(is.na(House.Prices.Kaggle.preprocessed))
# House.Prices.Kaggle.preprocessed[is.na(House.Prices.Kaggle.preprocessed$YrMoSoldCount) == T, c("YrMoSold")]
