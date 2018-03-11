three.month.window.stat.list <- list()


months <- sort(unique(user.labels$month))

# get all stat
for (m in c("2017-09-01", '2017-11-01')) {
  print(m)
  month.last <- as.Date(m)
  month(month.last) <- month(month.last) - 3
  
  transaction.pars <- dall[dall$month == m & dall$month > month.last,]
  transaction.pars <- transaction.pars[transaction.pars$id %in% user.labels$id[user.labels$month == m],]
  
  
  transaction.pars <- transaction.pars[order(transaction.pars$id, transaction.pars$date, transaction.pars$time, decreasing = T),]
  
  stat_three_month <- transaction.pars %>% group_by_(.dots = "id") %>% summarize(
    n_transactions = length(unique(id_trans)),
    
    sum_volume = sum(v_l),
    average_volume = mean(v_l),
    
    min_volume = min(v_l),
    max_volume = max(v_l),
    sd_volume = sd(v_l),
    
    sum_q = sum(q),
    average_q = mean(q),
    min_q = min(q),
    max_q = max(q),
    sd_q = sd(q),
    
    sum_sum_b = sum(sum_b),
    average_b = mean(sum_b),
    min_b = min(sum_b),
    max_b = max(sum_b),
    sd_b = sd(sum_b),
    
    sum_percent = sum(percent),
    average_percent = mean(percent),
    min_percent = min(percent),
    max_percent = max(percent),
    sd_percent = sd(percent),
    
    n_azs = length(unique(code_azs)),
    n_location = length(unique(location)),
    n_region = length(unique(region)),
    n_code = length(unique(code)),
    n_code1 = length(unique(code1)),
    n_types = length(unique(type)),
    
    average_period = mean(date_diff, na.rm = T),
    max_period = min(date_diff,  na.rm = T),
    min_period = max(date_diff, na.rm = T),
    sd_period = sd(date_diff, na.rm = T),
    
    average_diff_volume = mean(v_l - previous_v_l, na.rm = T),
    min_diff_volume = min(v_l - previous_v_l,  na.rm = T),
    max_diff_volume = max(v_l - previous_v_l, na.rm = T),
    sd_diff_volume = sd(v_l - previous_v_l, na.rm = T), 
    
    average_diff_period = mean(sum_b - previous_sum_b, na.rm = T),
    min_diff_period = min(sum_b - previous_sum_b,  na.rm = T),
    max_diff_period = max(sum_b - previous_sum_b, na.rm = T),
    sd_diff_period = sd(sum_b - previous_sum_b, na.rm = T), 
    
    average_diff_q = mean(q - previous_q, na.rm = T),
    min_diff_q = min(q - previous_q,  na.rm = T),
    max_diff_q = max(q - previous_q, na.rm = T),
    sd_diff_q = sd(q - previous_q, na.rm = T) 
    
  )
  
  stat_three_month$month <- m
  colnames(stat_three_month)[!colnames(stat_three_month) %in% c("month", "id")] <- 
    paste0("three_month_", colnames(stat_three_month)[!colnames(stat_three_month) %in% c("month", "id")])
  
  three.month.window.stat.list[[length(three.month.window.stat.list) + 1]] <- stat_three_month
}

three.month.stat <- rbindlist(three.month.window.stat.list)
three.month.stat[is.na(three.month.stat)] <- 0