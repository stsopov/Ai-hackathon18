# make predict to test

features.test <- dallFeatures[test.id,]
dtest <- xgb.DMatrix(data = as.matrix(features.test))

xgb.predict <- predict(clf.train, dtest)

pred.base <- data.frame(id = id[test.id], proba = xgb.predict)

submission.name = "xgb_submission"
write.csv(pred.base, paste0("Results/", submission.name, ".csv"), row.names = F,quote = F)
