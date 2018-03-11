last.year.window.stat.list <- list()


months <- sort(unique(user.labels$month))

# get all stat for month one year ago
for (m in c("2016-09-01", '2016-11-01')) {
  print(m)
  
  transaction.pars <- dall[dall$month == m,]
  transaction.pars <- transaction.pars[transaction.pars$id %in% user.labels$id[user.labels$month == m],]
  
  
  transaction.pars <- transaction.pars[order(transaction.pars$id, transaction.pars$date, transaction.pars$time, decreasing = T),]
  
  stat_three_month <- transaction.pars %>% group_by_(.dots = "id") %>% summarize(
    n_transactions = length(unique(id_trans)),
    n_transactions_non_zero_volume = length(unique(id_trans[v_l > 0])),
    n_transactions_zero_volume = length(unique(id_trans[v_l == 0])),
    n_transactions_zero_sum_b = length(unique(id_trans[sum_b == 0])),
    n_transactions_non_zero_percent = length(unique(id_trans[percent > 0])),
    
    sum_volume = sum(v_l),
    average_volume = mean(v_l),
    median_volume = median(v_l),
    min_volume = min(v_l),
    q25_volume = quantile(v_l, 0.25),
    q75_volume = quantile(v_l, 0.75),
    max_volume = max(v_l),
    sd_volume = sd(v_l),
    
    sum_q = sum(q),
    average_q = mean(q),
    median_q = median(q),
    min_q = min(q),
    q25_q = quantile(q, 0.25),
    q75_q = quantile(q, 0.75),
    max_q = max(q),
    sd_q = sd(q),
    
    sum_sum_b = sum(sum_b),
    average_b = mean(sum_b),
    median_b = median(sum_b),
    min_b = min(sum_b),
    q25_sum_b = quantile(sum_b, 0.25),
    q75_sum_b = quantile(sum_b, 0.75),
    max_b = max(sum_b),
    sd_b = sd(sum_b),
    
    sum_percent = sum(percent),
    average_percent = mean(percent),
    min_percent = min(percent),
    q25_percent = quantile(percent, 0.25),
    q75_percent = quantile(percent, 0.75),
    max_percent = max(percent),
    sd_percent = sd(percent),
    
    
    sum_percent_non_zero = sum(percent[percent > 0]),
    average_percent_non_zero = mean(percent[percent > 0]),
    min_percent_non_zero = min(percent[percent > 0]),
    q25_percent_non_zero = quantile(percent[percent > 0], 0.25),
    q75_percent_non_zero = quantile(percent[percent > 0], 0.75),
    max_percent_non_zero = max(percent[percent > 0]),
    sd_percent_non_zero = sd(percent[percent > 0]),
    
    n_azs = length(unique(code_azs)),
    n_azs_percent_non_zero = length(unique(code_azs[percent > 0])),
    n_azs_non_zero_volume = length(unique(code_azs[v_l > 0])),
    n_azs_non_zero_volume = length(unique(code_azs[v_l > 0])),
    
    n_location = length(unique(location)),
    n_location_percent_non_zero = length(unique(location[percent > 0])),
    n_location_non_zero_volume = length(unique(location[v_l > 0])),
    
    n_region = length(unique(region)),
    n_region_percent_non_zero = length(unique(region[percent > 0])),
    n_region_non_zero_volume = length(unique(region[v_l > 0])),
    
    n_code = length(unique(code)),
    n_code_percent_non_zero = length(unique(code[percent > 0])),
    n_code_non_zero_volume = length(unique(code[v_l > 0])),
    
    n_code1 = length(unique(code1)),
    n_code1_percent_non_zero = length(unique(code1[percent > 0])),
    n_code1_non_zero_volume = length(unique(code1[v_l > 0])),
    
    n_types = length(unique(type)),
    n_types_percent_non_zero = length(unique(type[percent > 0])),
    n_types_non_zero_volume = length(unique(type[v_l > 0])),
    
    average_period = mean(date_diff, na.rm = T),
    max_period = min(date_diff,  na.rm = T),
    q25_period = quantile(date_diff, 0.25, na.rm = T),
    q75_period = quantile(date_diff, 0.75, na.rm = T),
    min_period = max(date_diff, na.rm = T),
    sd_period = sd(date_diff, na.rm = T),
    
    sum_time_strage = sum(time == ""),
    
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
    sd_diff_q = sd(q - previous_q, na.rm = T), 
    
    # bonus
    bonus_got = sum(sum_b) * 0.04 - sum(percent)
    
  )
  
  stat_three_month$month <- m
  colnames(stat_three_month)[!colnames(stat_three_month) %in% c("month", "id")] <- 
    paste0("last_year_", colnames(stat_three_month)[!colnames(stat_three_month) %in% c("month", "id")])
  
  last.year.window.stat.list[[length(last.year.window.stat.list) + 1]] <- stat_three_month
}

last.year.stat <- rbindlist(last.year.window.stat.list)
last.year.stat[is.na(last.year.stat)] <- 0
last.year.stat$month <- gsub("2016", "2017", last.year.stat$month)

save(last.year.stat, file = "Data/prepared/last_year_stat")