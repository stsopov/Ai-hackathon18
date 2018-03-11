library(reshape2)

this.month.different.stat.list <- list()


months <- sort(unique(user.labels$month))

# get all stat
for (m in c("2017-09-01", '2017-11-01')) {
  print(m)
  transaction.pars <- dall[dall$month == m,]
  transaction.pars <- transaction.pars[transaction.pars$id %in% user.labels$id[user.labels$month == m],]
  
  transaction.pars <- transaction.pars[order(transaction.pars$id, transaction.pars$date, transaction.pars$time, decreasing = T),]
  transaction.pars <- transaction.pars[!duplicated(transaction.pars$id),]
  
  stat_this_month_different <- dcast(transaction.pars[, c("id", "region", "v_l")], id ~ region, sum)
  stat_this_month_different$month <- m
  
  colnames(stat_this_month_different)[!colnames(stat_this_month_different) %in% c("month", "id")] <- 
    paste0("location_", colnames(stat_this_month_different)[!colnames(stat_this_month_different) %in% c("month", "id")]) 
  
  
  this.month.different.stat.list[[length(this.month.different.stat.list) + 1]] <- stat_this_month_different
}

this.month.different.stat.list <- rbindlist(this.month.different.stat.list, fill = T)
this.month.different.stat.list[is.na(this.month.different.stat.list)] <- 0