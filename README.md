# hp2

This project `House Prices from Kaggle` uses ProjectTemplate package.

As a small test we used Azure ML side by side and ended up doing a large amount of preprocessing in Azure ML Studio.
Our experience was a bit mixed, some things are easy others are less straightforward. 
So for this project we start with the preprocessed file from Azure this included:
- recoding to ordered factors (especially quality assessments)
- simplified feature engineering like 'HasLotFrontage' or 'HasLandContour'
- fix missing values
- convert combined columns constructs like 'Condition1' and 'Condition2' to indicator values
- Normalize data Zscore
- SalePriceLog, MiscValLog
- Some extra small feature engineering like 'HouseDirectSoldAfterBuilt'
- Complete conversion to indicator values has not been applied


## Still some project reminders below.

1. Prepare Problem
a) Load libraries
b) Load dataset
c) Split-out validation dataset

2. Summarize Data
a) Descriptive statistics
b) Data visualizations

3. Prepare Data
a) Data Cleaning
b) Feature Selection
c) Data Transforms

4. Evaluate Algorithms
a) Test options and evaluation metric
b) Spot Check Algorithms
c) Compare Algorithms

5. Improve Accuracy
a) Algorithm Tuning
b) Ensembles

6. Finalize Model
a) Predictions on validation dataset
b) Create standalone model on entire training dataset
c) Save model for later use
