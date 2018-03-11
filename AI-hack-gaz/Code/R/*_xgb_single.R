# learn xgboost

dtrain <- xgb.DMatrix(data = as.matrix(dallFeatures[train.id, ]), label = TV[train.id])

#5 5 0.9 0.7 0.5379335
param <- list(  objective           = "binary:logistic",
                eval_metric = "auc",
                booster = "gbtree",
                eta                 = 0.03, # 0.06, #0.01,
                max_depth           = 3, #changed from default of 8\ best 3 
                gamma = 0,
                min_child_weight = 5,
                max_delta_step = 0,
                #subsample = 0.9,
                colsample_bytree = 0.9,
                reg_alpha = 5,
                silent = 1,
                nthread = 8
)


watchlist <- list(train = dtrain)

results.cv <- c()
#for(seed in c(12471, 123, 125412)) {
for(seed in c(1241271)) {
  set.seed(seed)
  
  clf <- xgb.cv(   params              = param,
                   data                = dtrain,
                   nrounds             = 2000, #- 180
                   verbose             = 1,
                   early.stop.round    = 150,
                   watchlist           = watchlist,
                   maximize            = T,
                   nfold = 5
  )
  
  res.fold <- max(clf$evaluation_log$test_auc_mean)
  print(res.fold)
  n_iter <- which.max(clf$evaluation_log$test_auc_mean)
  
  results.cv <- c(results.cv, res.fold)
}

print(results.cv)
print(mean(results.cv))

#print(results.cv)
#[1] 0.5375231 0.5376729 0.5375666
#> print(mean(results.cv))
#[1] 0.5375875


if(T) {
  set.seed(12471)
  clf.train <- xgb.train(   params              = param,
                            data                = dtrain,
                            nrounds             = n_iter,
                            verbose             = 1
  )
  
  imp.base <- xgb.importance(clf.train, feature_names = colnames(dallFeatures))
  View(imp.base)
}
