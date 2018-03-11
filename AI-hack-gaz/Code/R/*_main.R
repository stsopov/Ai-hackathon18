library(data.table)
library(dplyr)
library(xgboost)
library(textir)


# preparing data
source("Code/R/0_preparing.R")

# get features for all time
source("Code/R/1_get_features.R")

# get features for this month
source("Code/R/1_get_features_this_month.R")

# get features for this month last year
source("Code/R/1_get_features_last_year.R")

# merge all features
source("Code/R/1_final_feature_engineering.R")

# xgboost learn
source("Code/R/*_xgb_single.R")

# predict to test and make submission
source("Code/R/*_xgb_predict.R")