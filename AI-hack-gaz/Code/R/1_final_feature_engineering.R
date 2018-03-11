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

# load cash data

load("Data/prepared/all_stat")
load("Data/prepared/dall")
load("Data/prepared/user_labels")
load("Data/prepared/this_month_stat")
load("Data/prepared/last_year_stat")

all.stat$days_passed_after_first_p <- as.numeric(difftime(all.stat$month, 
                                                          all.stat$f_date, units = "days"))

all.stat <- merge(all.stat, user.labels, by = c("id", "month"))
all.stat <- all.stat[all.stat$month != '2017-12-01',]

dallFeatures <- all.stat


# get dummy variables
dummyMonth <- getDummyVar(dallFeatures, "month", 0 )
dallFeatures <- cbind(dallFeatures, dummyMonth)

dummyFMonth <- getDummyVar(dallFeatures, "f_month", 1000 )
dallFeatures <- cbind(dallFeatures, dummyFMonth)


if (F) {
  train.miha <- read.csv("Data/prepared/gpn_train_data_last_3_mon_y_v4_with_split_id.csv", stringsAsFactors = F)
  test.miha <- read.csv("Data/prepared/gpn_test_data_x_v4.csv", stringsAsFactors = F)
  
  train.miha$month <- ifelse(train.miha$end_time == "1506805200", '2017-09-01', 
                             ifelse(train.miha$end_time == "1509397200", '2017-10-01', '2017-11-01'))
  test.miha$month <- ifelse(test.miha$end_time == "1506805200", '2017-09-01', '2017-11-01')
  
  train.miha <- train.miha[, order(colnames(train.miha))]
  train.miha$ll <- NULL
  train.miha$y <- NULL
  test.miha <- test.miha[, order(colnames(test.miha))]
  
  all.miha <- rbind(train.miha, test.miha)
  all.miha[,c('y', 'll', 'max_date_purchase', 'end_time', 'min_fp','tt','id_1','id_3','id_2')] <- NULL
  
  all.miha$end_time <- NULL
  all.miha[['tt1']]=all.miha[['recency_days']]/all.miha[['mean_period']]
  all.miha[['tt2']]=all.miha[['recency_days']]-all.miha[['mean_period']]
  all.miha[['tt3']]=all.miha[['current_period']]/all.miha[['mean_period']]
  all.miha[['tt4']]=all.miha[['revenue']]*0.04-all.miha[['sum_percent']]
  all.miha[['tt5']]=all.miha[['churn']]*all.miha[['recency_days']]
  all.miha[['tt6']]=all.miha[['month_churn']]*all.miha[['churn']]
  all.miha[['tt7']]=all.miha[['purchases']]*all.miha[['differ_azs']]
  all.miha[['tt8']]=all.miha[['last_volume']]/all.miha[['avpu']]
  all.miha[['tt9']]=all.miha[['purchases']]/all.miha[['lt']]
  
  
  colnames(all.miha)[!colnames(all.miha) %in% c("id", "month")] <- paste0(colnames(all.miha)[!colnames(all.miha) %in% c("id", "month")], 
                                                                          "_miha")
  
  dallFeatures <- merge(dallFeatures, all.miha, by = c("id", "month"))
}

dallFeatures <- merge(dallFeatures, this.month.stat, by = c("id", "month"))
dallFeatures <- merge(dallFeatures, last.year.stat, by = c("id", "month"), all.x = T)

# remove some data
dallFeatures[, c("f_month", "next_month", "month_diff", 
                 "last_purchase_date", "f_date")] <- NULL

# define train and test id
train.id <- which(dallFeatures$is_test == 0 & 
                    dallFeatures$month %in% c("2017-09-01", "2017-11-01") # & 
                  # dallFeatures$id %in% train_data$id
)

test.id <- which(dallFeatures$is_test == 1 & 
                   dallFeatures$month %in% c("2017-09-01", "2017-11-01"))

TV <- dallFeatures$TV
id <- dallFeatures$id
dallFeatures[, c("id", "month", "is_test", "TV")] <- NULL

# some preparing and adding one feature
dallFeatures$f_1 <- dallFeatures$day_last_purchase + dallFeatures$average_period
dallFeatures[is.na(dallFeatures)] <- 0
dallFeatures[(dallFeatures == -Inf)] <- 0
dallFeatures[(dallFeatures == Inf)] <- 0

dallFeatures$last_code <- NULL
dallFeatures$last_code1 <- NULL
