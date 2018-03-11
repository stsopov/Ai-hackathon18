library(h2o)

getROC_AUC = function(probs, true_Y){
  probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
  val = unlist(probsSort$x)
  idx = unlist(probsSort$ix)
  roc_y = true_Y[idx];
  stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
  stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)
  auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
  return(list(stack_x=stack_x, stack_y=stack_y, auc=auc))
}

normalize <- function(x)
{
  x <- x - min(x)
  x / max(x)
}


getDummyVar <- function(dataset, par, min_freq) {
  par.value <- dataset[[par]]
  par.value[is.na(par.value)] <- "MISSED"
  
  par_freq <- table(par.value)
  popular_pars <- names(par_freq)[par_freq >= min_freq]
  
  par_prep <- dataset[[par]]
  
  par_prep[which(!par_prep %in% popular_pars)] <- "other"
  
  dummyPar <- as.matrix(sparse.model.matrix(~-1 + ., data = data.frame(dummy.par = as.factor(par_prep))))
  colnames(dummyPar) <- paste0(par, "__",1:ncol(dummyPar))
  
  dummyPar
}


dallFeaturesNN <- dallFeatures[, c("this_month_n_transactions_non_zero_volume", "sd_period"                       ,         
                                   "this_month_average_period",                 "this_month_n_transactions"      ,          
                                   "min_period" ,                               "day_last_purchase"             ,           
                                   "n_transactions"  ,                          "month__1"                     ,            
                                   "this_month_q25_period"   ,                  "this_month_min_period"         ,           
                                   "f_1"                    ,                   "this_month_sum_volume"         ,           
                                   "q75_percent"            ,                   "n_transactions_non_zero_volume",           
                                   "this_month_q75_period", "average_period", 
                                   "sum_sum_b", "sum_sum_b", "this_month_sd_period", "n_month_was")]

dummyDay <- getDummyVar(dallFeaturesNN, "day_last_purchase", 1000 )
dallFeaturesNN <- cbind(dallFeaturesNN, dummyDay)

dummyMonthWas <- getDummyVar(dallFeaturesNN, "n_month_was", 1000 )
dallFeaturesNN <- cbind(dallFeaturesNN, dummyMonthWas)

for (j in 1:ncol(dallFeaturesNN)) {
  dallFeaturesNN[[j]] <- normalize(dallFeaturesNN[[j]])
}


h2o.init()

set.seed(124)
id <- train.id

nfolds = 5
set.seed(17)

final.nn.list <- list()

for (seed in c(1241252)) {
  preds.base.train <- data.frame(predict = rep(0, length(id)), id = id, TV = 0)
  res.list <- list()
  
  for (i in 1:nfolds) {
    id.fold <- id[which((id %% nfolds + 1) != i)]
    id.cv <- id[which((id %% nfolds + 1) == i)]
    
    train <- data.frame(dallFeaturesNN[id.fold,])
    test <- data.frame(dallFeaturesNN[id.cv, ])
    
    train$TV <- as.factor(TV[id.fold])
    test$TV <- as.factor(TV[id.cv])
    
    
    train.hex <- as.h2o(train)
    test.hex <- as.h2o(test)
    
    predictors <- 1:(ncol(train.hex)-1)
    response <- ncol(train.hex)
    
    print(i)
    model <- h2o.deeplearning(x=predictors,
                              y=response,
                              training_frame = train.hex,
                              activation="RectifierWithDropout",
                              hidden= scheme,
                              input_dropout_ratio=0.07,
                              epochs=50,
                              l1=5e-05,
                              l2=1e-3,
                              rho=0.99,
                              epsilon=1e-8,
                              stopping_metric = "AUC",
                              max_w2=5,
                              reproducible = T,
                              seed=seed)
    if (i == 1) print(model)
    
    preds <- as.data.frame(h2o.predict(model,test.hex))$p1
    print(getROC_AUC(preds, TV[id.cv])$auc)
    
    preds.base.train$predict[which(id %in% id.cv)] <- preds
    preds.base.train$TV[which(id %in% id.cv)] <- TV[id.cv]
  }
  
  train.preds <- preds.base.train$predict
  final.res <- getROC_AUC(preds.base.train$predict,preds.base.train$TV)$auc
  print(final.res)
  
  
  ###############
  # FINAL TRAIN #
  ###############
  
  train <- data.frame(dallFeaturesNN[train.id,])
  test <- data.frame(dallFeaturesNN[test.id, ])
  
  train$TV <- as.factor(TV[train.id])
  
  train.hex <- as.h2o(train)
  test.hex <- as.h2o(test)
  
  predictors <- 1:(ncol(train.hex)-1)
  response <- ncol(train.hex)
  
  model <- h2o.deeplearning(x=predictors,
                            y=response,
                            training_frame = train.hex,
                            activation="RectifierWithDropout",
                            hidden= c(150, 150),
                            input_dropout_ratio=0.07,
                            epochs=50,
                            l1=5e-05,
                            l2=1e-3,
                            rho=0.99,
                            epsilon=1e-8,
                            stopping_metric = "AUC",
                            max_w2=5,
                            reproducible = T,
                            seed=seed)
  
  
  test.preds <- as.data.frame(h2o.predict(model, test.hex))$p1
  
  
  preds.seed <- data.frame(nn_predict = c(train.preds, test.preds), id = c(train.id, test.id))
  final.nn.list[[length(final.nn.list) + 1]] <- preds.seed
}

final.nn <- do.call(rbind, final.nn.list)

pred.base <- read.csv("Results/first_catboost_8265.csv")
pred.base$proba <- final.nn$nn_predict[final.nn$id %in% test.id]
submission.name = "nn_825"
write.csv(pred.base, paste0("Results/", submission.name, ".csv"), row.names = F,quote = F)
