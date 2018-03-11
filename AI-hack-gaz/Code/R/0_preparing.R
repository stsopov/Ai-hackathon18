library(data.table)
library(dplyr)
library(xgboost)
library(textir)


train_data <- fread("Data/raw/train_data.csv", sep = ",", stringsAsFactors = F)
test_data <- fread("Data/raw/test_data.csv", sep = ",", stringsAsFactors = F)

dall <- rbind(train_data, test_data)
dall$V1 <- NULL
dall$id_trans <- 1:nrow(dall)

dall$month <- paste0(substr(dall$date, 1, 8), "01")
dall <- dall[!is.na(dall$sum_b),]

# некоторая чистка данных
dall <- dall[-which(dall$percent == 0 & dall$sum_b == 0),]

dall$code_azs[is.na(dall$code_azs)] <- 99999
dall$region[is.na(dall$region)] <- 469
dall$percent[is.na(dall$percent)] <- 0
dall$code1[is.na(dall$code1)] <- 0
dall$location[is.na(dall$location)] <- 0
dall <- dall[dall$q >= 0 & dall$sum_b >= 0 & dall$v_l >= 0 & dall$percent >= 0,]

# проставляем лейблы
user.labels <- unique(dall[dall$sum_b > 0, c("id", "month"), with = F])
user.labels <- user.labels[order(user.labels$id, user.labels$month),]
user.labels[, next_month := shift(month, 1L, type = "lead"), by = "id"]
user.labels$month_diff <- as.numeric(difftime(user.labels$next_month, user.labels$month, units = "days"))
user.labels$TV <- as.numeric(user.labels$month_diff <= 31)
user.labels$TV[is.na(user.labels$TV)] <- 0
user.labels$key <- paste0(user.labels$id, user.labels$month)

test.labels <- user.labels[user.labels$id %in% test_data$id,] %>% group_by_(.dots = "id") %>% 
  summarize(max_month = max(month))
test.labels$key <- paste0(test.labels$id, test.labels$max_month)


user.labels$is_test <- as.numeric(user.labels$key %in% test.labels$key)
user.labels$key <- NULL

save(dall, file = "Data/prepared/dall")
save(user.labels, file = "Data/prepared/user_labels")
