# # Aggregating readings ----------
# library(dplyr)
# library(readr)
# library(lubridate)
# # set working directory to data folder - Contains all: A-S cleaned data files, as per 09.05.2021
# setwd("~/Desktop/Data Folder - StatsLab - May 21")
# my_repo <- "~/Documents/ETH/Stats Lab/"
# source(paste0(my_repo, "polytrauma/code/preprocessing/helper_functions.R"))
# 
# # Aggregating between surgeries ---------
# get_statistics <- function(x, op = surgery_times, var, time_var, 
#                            t1 = NA, t2 = "start_1", surgery = 1, op_only = 1, hosp = hospital) {
#   # browser()
#   # x: dataframe to be analysed
#   # op: dataframe containing all times between surgeries (timepoints)
#   # var: variable name with repeated measurements
#   # time_var: name of time_var which identifiers which time reading is taken
#   # t1: time 0 at which to start aggregating 
#   # t2: time at which to stop aggregating
#   # surgery: the number of the surgery being processed
#   # op_only: when 1, look at those who had surgeries. When 0, look at those who died before surgery
# 
#   
#   # FIX - deal with those who died without surgery separately using op_only! I.e. time 0 - death -----
#   # any deaths after surgery will be accounted for (i.e. surgery - death, or next surgery won't be there)
#   if (op_only==1) {
#     x <- merge(x, op, by = "research_case_id") # keep only those which are present in both
#     
#     if (class(x[[var]])=="numeric" | class(x[[var]])=="integer") {
#       # hist(x[[var]], xlab = var, main = NA, breaks = 30) # a histogram 
#       
#       if (is.na(t1)) {
#         temp <- x %>% select(research_case_id, .data[[time_var]], .data[[var]], .data[[t2]]) %>%
#           group_by(research_case_id) %>% 
#           filter(!is.na(.data[[t2]])) %>% # must not be NA for the timepoint, otherwise no surgery to aggregate
#           filter(!is.na(.data[[var]])) %>% # only look at non NA!
#           filter(.data[[time_var]] <= .data[[t2]]) %>% # only look bw the two time points
#           summarise(surgery_no = surgery,
#                     max = max(.data[[var]], na.rm = TRUE), 
#                     min = min(.data[[var]], na.rm = TRUE), 
#                     med = median(.data[[var]], na.rm = TRUE), 
#                     mean = mean(.data[[var]], na.rm = TRUE), 
#                     log_mean = mean(log(.data[[var]]), na.rm = TRUE), #
#                     sd = sd(.data[[var]], na.rm = TRUE), 
#                     lq = quantile(.data[[var]], 0.25, na.rm = TRUE), 
#                     uq = quantile(.data[[var]], 0.75, na.rm = TRUE),
#                     first = head(.data[[var]], 1, na.rm = TRUE),
#                     last = tail(.data[[var]], 1, na.rm = TRUE),
#                     n = n() , .groups = 'keep')
#       }  else {
#         temp <- x %>% select(research_case_id, .data[[time_var]], .data[[var]], .data[[t1]], .data[[t2]]) %>%
#           group_by(research_case_id) %>% 
#           filter(!is.na(t2)) %>% # must not be NA for the timepoint, otherwise no surgery to aggregate
#           filter(!is.na(.data[[var]])) %>% # only look at non NA!
#           filter(.data[[time_var]] >= .data[[t1]] & .data[[time_var]] <= .data[[t2]]) %>% # only look bw the two time points
#           summarise(surgery_no = surgery,
#                     max = max(.data[[var]], na.rm = TRUE), 
#                     min = min(.data[[var]], na.rm = TRUE), 
#                     med = median(.data[[var]], na.rm = TRUE), 
#                     mean = mean(.data[[var]], na.rm = TRUE), 
#                     log_mean = mean(log(.data[[var]]), na.rm = TRUE), #
#                     sd = sd(.data[[var]], na.rm = TRUE), 
#                     lq = quantile(.data[[var]], 0.25, na.rm = TRUE), 
#                     uq = quantile(.data[[var]], 0.75, na.rm = TRUE),
#                     first = head(.data[[var]], 1, na.rm = TRUE),
#                     last = tail(.data[[var]], 1, na.rm = TRUE),
#                     n = n(), .groups = 'keep') # statistics we look at. Could be added to.
#       }
# 
#       # renaming the columns: 2:11 (10 stats)
#       names(temp)[3:13] <- sapply(names(temp)[3:13], function(x) {paste0(var, "_",x)})
#       
#     }  else if (class(x[[var]])=="factor") {
#       temp <- x %>% select(research_case_id, .data[[time_var]], .data[[var]], .data[[t2]]) %>%
#         group_by(research_case_id) %>% 
#         filter(!is.na(t2)) %>% # must not be NA for the timepoint, otherwise no surgery to aggregate
#         filter(!is.na(.data[[var]])) %>% # only look at non NA!
#         filter(.data[[time_var]] <= .data[[t2]]) %>% # only look bw the two time points
#         # could potentially look at a different time variable (e.g. time of admission)
#         summarise(surgery_no = surgery,
#                   mode = names(which.max(table(.data[[var]]))),
#                   first = head(.data[[var]], 1),
#                   last = tail(.data[[var]], 1),
#                   n = n(), .groups = 'keep') # statistics we look at. Could be added to.
#       
#       # renaming the columns: 2: mode
#       names(temp)[3:6] <- sapply(names(temp)[3:6], function(x) {paste0(var, "_",x)})
#     }
#   } else if (op_only==0) {
#     # now we look at those who died without surgery!
#     without_surgery_ID <- unique(x$research_case_id[! x$research_case_id %in% op$research_case_id ])
#     hosp_death <- hosp %>% 
#       mutate(death = ifelse(is.na(Tod_Eingetroffen_Datumzeit), 0, 1), .keep = 'all') %>% 
#       filter(death==1)
#     
#     dead_ID <- unique(without_surgery_ID[without_surgery_ID %in% hosp_death$research_case_id ])
#     
#     if (class(x[[var]])=="numeric"| class(x[[var]])=="integer") {
#       
#       temp <- x %>% select(research_case_id, .data[[time_var]], .data[[var]]) %>%
#         group_by(research_case_id) %>% 
#         filter(research_case_id %in% dead_ID) %>% # must not be NA for the timepoint, otherwise no surgery to aggregate
#         filter(!is.na(.data[[var]])) %>% # only look at non NA!
#         summarise(surgery_no = NA,
#                   max = max(.data[[var]], na.rm = TRUE), 
#                   min = min(.data[[var]], na.rm = TRUE), 
#                   med = median(.data[[var]], na.rm = TRUE), 
#                   mean = mean(.data[[var]], na.rm = TRUE), 
#                   log_mean = mean(log(.data[[var]]), na.rm = TRUE), #
#                   sd = sd(.data[[var]], na.rm = TRUE), 
#                   lq = quantile(.data[[var]], 0.25, na.rm = TRUE), 
#                   uq = quantile(.data[[var]], 0.75, na.rm = TRUE),
#                   first = head(.data[[var]], 1, na.rm = TRUE),
#                   last = tail(.data[[var]], 1, na.rm = TRUE),
#                   n = n() , .groups = 'keep')
#       
#       # renaming the columns: 2:11 (10 stats)
#       names(temp)[3:13] <- sapply(names(temp)[3:13], function(x) {paste0(var, "_",x)})
#     }  else if (class(x[[var]])=="factor") {
#       temp <- x %>% select(research_case_id, .data[[time_var]], .data[[var]]) %>%
#         group_by(research_case_id) %>% 
#         filter(research_case_id %in% dead_ID) %>% # must not be NA for the timepoint, otherwise no surgery to aggregate
#         filter(!is.na(.data[[var]])) %>% # only look at non NA!
#         summarise(surgery_no = NA,
#                   mode = names(which.max(table(.data[[var]]))),
#                   first = head(.data[[var]], 1),
#                   last = tail(.data[[var]], 1),
#                   n = n() , .groups = 'keep') # statistics we look at. Could be added to.
#       
#       # renaming the columns: 2: mode
#       names(temp)[3:6] <- sapply(names(temp)[3:6], function(x) {paste0(var, "_",x)})
#     }
#   }
#   temp
# }
# # currently: if no info, gives - inf as max, inf as min and NA or NaN for the other values
# 
# # Applications to the repeated measurement variables -----------
# # Other datasets needed -----------
# # Will be helpful to pull data together in the end
# # will need the operations timings dataset
# load("H_Operationen_Surgery_Times_7164.RData")
# 
# max_op <- (ncol(surgery_times)-1)/2
# 
# hospital <- read.csv("I_Patient_Unfall.csv", stringsAsFactors = T)
# hospital <- filter_dataset(hospital)
# 
# # start with bga: ---------------
# 
# bga <- read.csv("A_Bga_Werte.csv", stringsAsFactors = T)
# bga <- filter_dataset(bga) %>% mutate(Messung_dttm = ymd_hms(Messung_dttm))
# not_all_NA <- names(bga)[sapply(names(bga), function(x) {!all(is.na(bga[[x]]))})]
# bga <- bga[, not_all_NA]
# 
# bga.vars <-  names(bga)[4:ncol(bga)] # the variables names with repeated observations. 
# 
# op_n <- surgery_times %>% mutate(n = (rowSums(!is.na(surgery_times)) - 1)/2 ) %>%
#   select(research_case_id, n)
# df <- data.frame()  
# 
# df <- op_n %>% filter(n >= 1) %>% select(research_case_id) %>% mutate(surgery_no = 1)
# for (i in 1:length(bga.vars)) {
#   temp <- get_statistics(x= bga, var = bga.vars[i], time_var = names(bga)[3])
#   if (dim(temp)[1]!=0) {
#     df <- left_join(df, temp, by = c("research_case_id", "surgery_no")) 
#   }
#   #else (print(paste0("Empty column: ", bga.vars[i])))
# } # all columns should now be present!
# 
# #temporary_df <- merge(bga, op_n, by ="research_case_id") %>% filter(n>=1)
# # keeps only the IDs in both frames. What we want
# #summary(temporary_df$Anion.gap..K..) # only NA! Very strange 
# 
# # other surgeries:
# 
# for (j in 2:max_op) {
#   df2 <- op_n %>% filter(n >= j) %>% select(research_case_id) %>% mutate(surgery_no = j)
#   for (i in 1:length(bga.vars)) {
#     temp <- get_statistics(x = bga, var = bga.vars[i], time_var = names(bga)[3], 
#                            surgery = j, t1 = paste0("end_",j-1), t2 = paste0("start_",j))
#     if (dim(temp)[1]!=0) {
#       df2 <- left_join(df2, temp, by = c("research_case_id", "surgery_no"))
#     } 
#     #else (print(paste0("Empty column: ", bga.vars[i])))
#   }
#   df <- bind_rows(df, df2)
# } # should be 8322 rows!
# 
# # if surgery_no == NA: it means the person died before surgery
# 
# # for those who died: 
# without_surgery_ID <- unique(bga$research_case_id[! bga$research_case_id %in% surgery_times$research_case_id ])
# 
# hosp_death <- hospital %>% 
#   mutate(death = ifelse(is.na(Tod_Eingetroffen_Datumzeit), 0, 1), .keep = 'all') %>% 
#   filter(death==1)
# 
# dead_ID <- unique(without_surgery_ID[without_surgery_ID %in% hosp_death$research_case_id ])
# 
# df_dead <- bind_cols(research_case_id = dead_ID, surgery_no = NA)
# 
# for (i in 1:length(bga.vars)) {
#   temp <- get_statistics(x= bga, var = bga.vars[i], time_var = names(bga)[3], op_only = 0, surgery = NA)
#   
#   if (dim(temp)[1]!=0) {
#     df_dead <- left_join(df_dead, temp, by = c("research_case_id", "surgery_no")) 
#     
#   }
#   #else (print(paste0("Empty column: ", bga.vars[i])))
# } 
# 
# 
# bga.df <- bind_rows(df, df_dead)
# summary(bga.df$surgery_no)
# 
# 
# write.csv(bga.df, "Bga_Werte_Aggregate.csv", row.names = F)
# 
# # next: blut --------------
# blut <- read.csv("B_Blut_Werte_Categorized.csv", stringsAsFactors = T)
# blut <- filter_dataset(blut) %>% mutate(ObservationDtTm = ymd_hms(ObservationDtTm))
# not_all_NA <- names(blut)[sapply(names(blut), function(x) {!all(is.na(blut[[x]]))})]
# blut <- blut[, not_all_NA]
# 
# blut.vars <-  names(blut)[4:ncol(blut)] # the variables names with repeated observations. 
# 
# op_n <- surgery_times %>% mutate(n = (rowSums(!is.na(surgery_times)) - 1)/2 ) %>%
#   select(research_case_id, n)
# df <- data.frame()  
# 
# df <- op_n %>% filter(n >= 1) %>% select(research_case_id) %>% mutate(surgery_no = 1)
# for (i in 1:length(blut.vars)) {
#   temp <- get_statistics(x= blut, var = blut.vars[i], time_var = names(blut)[3])
#   if (dim(temp)[1]!=0) {
#     df <- left_join(df, temp, by = c("research_case_id", "surgery_no")) 
#   }
#   #else (print(paste0("Empty column: ", blut.vars[i])))
# } # all columns should now be present!
# 
# 
# # other surgeries:
# 
# for (j in 2:max_op) {
#   df2 <- op_n %>% filter(n >= j) %>% select(research_case_id) %>% mutate(surgery_no = j)
#   for (i in 1:length(blut.vars)) {
#     temp <- get_statistics(x = blut, var = blut.vars[i], time_var = names(blut)[3], 
#                            surgery = j, t1 = paste0("end_",j-1), t2 = paste0("start_",j))
#     if (dim(temp)[1]!=0) {
#       df2 <- left_join(df2, temp, by = c("research_case_id", "surgery_no"))
#     } 
#     #else (print(paste0("Empty column: ", blut.vars[i])))
#   }
#   df <- bind_rows(df, df2)
# } # should be 8322 rows!
# 
# # if surgery_no == NA: it means the person died before surgery
# 
# # for those who died: 
# without_surgery_ID <- unique(blut$research_case_id[! blut$research_case_id %in% surgery_times$research_case_id ])
# 
# hosp_death <- hospital %>% 
#   mutate(death = ifelse(is.na(Tod_Eingetroffen_Datumzeit), 0, 1), .keep = 'all') %>% 
#   filter(death==1)
# 
# dead_ID <- unique(without_surgery_ID[without_surgery_ID %in% hosp_death$research_case_id ])
# 
# df_dead <- bind_cols(research_case_id = dead_ID, surgery_no = NA)
# 
# for (i in 1:length(blut.vars)) {
#   temp <- get_statistics(x= blut, var = blut.vars[i], time_var = names(blut)[3], op_only = 0, surgery = NA)
#   
#   if (dim(temp)[1]!=0) {
#     df_dead <- left_join(df_dead, temp, by = c("research_case_id", "surgery_no")) 
#     
#   }
#   #else (print(paste0("Empty column: ", blut.vars[i])))
# } 
# 
# 
# blut.df <- bind_rows(df, df_dead)
# summary(blut.df$surgery_no)
# 
# 
# write.csv(blut.df, "Blut_Werte_Aggregate.csv", row.names = F)
# 
# 
# 
# 
# # final: vitals -------------
# vitals <- read.csv("R_Vital_Werte.csv", stringsAsFactors = T)
# vitals <- filter_dataset(vitals) %>% mutate(STARTDAT = ymd_hms(STARTDAT))
# 
# vitals.vars <-  levels(vitals$Messung_Name)
# 
# op_n <- surgery_times %>% mutate(n = (rowSums(!is.na(surgery_times)) - 1)/2 ) %>%
#   select(research_case_id, n)
# df <- data.frame()  
# 
# df <- op_n %>% filter(n >= 1) %>% select(research_case_id) %>% mutate(surgery_no = 1)
# for (i in 1:length(vitals.vars)) {
#   vitals_sub <- vitals %>% filter(Messung_Name==vitals.vars[i])
#   temp <- get_statistics(x= vitals_sub, var = "Messung_Wert", time_var = "STARTDAT")
#   names(temp) <- gsub("Messung_Wert", vitals.vars[i], names(temp))
#   if (dim(temp)[1]!=0) {
#     df <- left_join(df, temp, by = c("research_case_id", "surgery_no")) 
#   }
#   #else (print(paste0("Empty column: ", vitals.vars[i])))
# } # all columns should now be present!
# 
# # other surgeries:
# 
# for (j in 2:max_op) {
#   df2 <- op_n %>% filter(n >= j) %>% select(research_case_id) %>% mutate(surgery_no = j)
#   for (i in 1:length(vitals.vars)) {
#     vitals_sub <- vitals %>% filter(Messung_Name==vitals.vars[i])
#     temp <- get_statistics(x= vitals_sub, var = "Messung_Wert", time_var = "STARTDAT", 
#                            surgery = j, t1 = paste0("end_",j-1), t2 = paste0("start_",j))
#     names(temp) <- gsub("Messung_Wert", vitals.vars[i], names(temp))
#     if (dim(temp)[1]!=0) {
#       df2 <- left_join(df2, temp, by = c("research_case_id", "surgery_no"))
#     } 
#     #else (print(paste0("Empty column: ", vitals.vars[i])))
#   }
#   df <- bind_rows(df, df2)
# } # should be 8322 rows!
# 
# # if surgery_no == NA: it means the person died before surgery
# 
# # for those who died: 
# without_surgery_ID <- unique(vitals$research_case_id[! vitals$research_case_id %in% surgery_times$research_case_id ])
# 
# hosp_death <- hospital %>% 
#   mutate(death = ifelse(is.na(Tod_Eingetroffen_Datumzeit), 0, 1), .keep = 'all') %>% 
#   filter(death==1)
# 
# dead_ID <- unique(without_surgery_ID[without_surgery_ID %in% hosp_death$research_case_id ])
# 
# df_dead <- bind_cols(research_case_id = dead_ID, surgery_no = NA)
# 
# for (i in 1:length(vitals.vars)) {
#   vitals_sub <- vitals %>% filter(Messung_Name==vitals.vars[i])
#   temp <- get_statistics(x= vitals_sub, var = "Messung_Wert", time_var = "STARTDAT", 
#                          op_only = 0, surgery = NA)
#   names(temp) <- gsub("Messung_Wert", vitals.vars[i], names(temp))
#   if (dim(temp)[1]!=0) {
#     df_dead <- left_join(df_dead, temp, by = c("research_case_id", "surgery_no")) 
#     
#   }
#   #else (print(paste0("Empty column: ", vitals.vars[i])))
# } 
# 
# 
# vitals.df <- bind_rows(df, df_dead)
# summary(vitals.df$surgery_no)
# 

#write.csv(vitals.df, "Vitals_Werte_Aggregate.csv", row.names = F)

# NEW - more efficient!: -----------

# Aggregating readings ----------
# functions ----
get_statistics <- function(x, op = surgery_times, var, time_var) {
  # browser()
  # x: dataframe to be analysed
  # op: dataframe containing all times between surgeries (timepoints)
  # vars: variable name with repeated measurements
  # time_var: name of time_var which identifiers which time reading is taken
  var_factor <- names(x[, var])[sapply(x[, var], class)=='factor']
  var <- names(x[, var])[sapply(x[, var], class)=='numeric']
  # first surgery:
  first_surgery <- op %>% filter(SurgeryNr==1)
  x_factor <- x %>% select(research_case_id, time_var, var_factor)
  x <- x %>% select(research_case_id, time_var, var)
  calculate_mode <- function(x) {
    uniqx <- unique(x)
    uniqx[which.max(tabulate(match(x, uniqx)))]
  }

  get_first <- function(y, na.rm = TRUE) {
    first <- y[which(!is.na(y))[1]]
    if (is.na(first)) {return (NA)}
    first
  }
  
  get_last <- function(y, na.rm = TRUE) {
    if (length(which(!is.na(y))) > 0) {
      last <- y[which(!is.na(y))[length(which(!is.na(y)))]]
      return(last)
    }    
    return(NA)
  }
  
  my_sd <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {return(NA)}
    if (sum(!is.na(x))==1) {return(0)}
    else return(sd(x, na.rm = TRUE))
  }
  
  my_skewness <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {return(NA)}
    if (sum(!is.na(x))==1) {return(0)}
    else return(skewness(x, na.rm = TRUE))
  }
  
  my_kurtosis <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {return(NA)}
    if (sum(!is.na(x))==1) {return(0)}
    else return(kurtosis(x, na.rm = TRUE))
  }
  
  factors_agg <- first_surgery %>% 
    left_join(x_factor, by = 'research_case_id') %>%  
    filter(.data[[time_var]]<= start) %>% # this steps loses out on a lot of obs! (i.e. many don't have any readings before start of surgery!)
    group_by(research_case_id, SurgeryNr) %>% 
    summarise_at(var_factor, funs(mode = calculate_mode(.),
                                  n = sum(!is.na(.)), first = get_first(.),
                                  last = get_last(.)))
  #ames(factors_agg)[3:ncol(factors_agg)] <- sapply(var_factor, function(y) {sapply(names(factors_agg)[3:6], 
  #                                                             function(x) {paste0(y, "_",x)})})
  aggregated <-  first_surgery %>% 
    left_join(x, by = 'research_case_id') %>%  
    filter(.data[[time_var]]<= start) %>% # this steps loses out on a lot of obs! (i.e. many don't have any readings before start of surgery!)
    group_by(research_case_id, SurgeryNr) %>% 
    summarise_at(var, funs(max, min, median, mean, sd = my_sd(.), 
                           n = sum(!is.na(.)), first = get_first(.), 
                           last = get_last(.),
                           lq = quantile(., prob = 0.25), uq = quantile(., prob = 0.75),
                           skew = my_skewness(.), kurtosis = my_kurtosis(.)), 
                 na.rm = TRUE) 
  

  
  new_op <- data.frame()
  for (i in 2:max(op$SurgeryNr)) {
    prev_end <- op %>%
                filter(SurgeryNr == i-1) %>% 
                select(research_case_id, SurgeryNr, end)
    names(prev_end)[3] <- 'prev_end'
    surgery_i <- op %>% filter(SurgeryNr == i)
    temp <- left_join(surgery_i, prev_end[, -2], by = 'research_case_id')
    new_op <- bind_rows(new_op, temp)
  }
  
  aggregated_2_factors <-  new_op %>% group_by(research_case_id) %>% 
    left_join(x_factor, by = 'research_case_id') %>% 
    filter(.data[[time_var]]<= start & .data[[time_var]]>= prev_end) %>%
    group_by(research_case_id, SurgeryNr) %>% 
    summarise_at(var_factor, funs(mode = calculate_mode(.),
                                  n = sum(!is.na(.)), first = get_first(.),
                                  last = get_last(.)))
  #names(aggregated_2_factors)[3:6] <- sapply(var_factor, function(y) {
  #          sapply(names(aggregated_2_factors)[3:6], 
  #                                      function(x) {paste0(y, "_",x)})})
  aggregated_2 <-  new_op %>% group_by(research_case_id) %>% 
    left_join(x, by = 'research_case_id') %>% 
    filter(.data[[time_var]]<= start & .data[[time_var]]>= prev_end) %>%
    group_by(research_case_id, SurgeryNr) %>% 
    summarise_at(var, funs(max, min, median, mean, sd = my_sd(.), 
                           n = sum(!is.na(.)), first = get_first(.), 
                           last = get_last(.),
                           lq = quantile(., prob = 0.25), uq = quantile(., prob = 0.75),
                           skew = my_skewness(.), kurtosis = my_kurtosis(.)), 
                 na.rm = TRUE) 
  
  if (length(var_factor)>0) {
    aggregated <- aggregated %>% 
      left_join(factors_agg, by = c('research_case_id', 'SurgeryNr'))
    aggregated_2 <- aggregated_2 %>% 
      left_join(aggregated_2_factors, by = c('research_case_id', 'SurgeryNr'))
  }
  
  aggregated_final <- bind_rows(aggregated, aggregated_2)
  aggregated_final 
}

get_statistics_died <- function(x, var) {
  #browser()
  # x: dataframe to be analysed
  # no time variable needed: all obs can be aggregated, as patient dies before any surgery
  # op: dataframe containing all times between surgeries (timepoints)
  # vars: variable name with repeated measurements
  # time_var: name of time_var which identifiers which time reading is taken
  var_factor <- names(x[, var])[sapply(x[, var], class)=='factor']
  var <- names(x[, var])[sapply(x[, var], class)=='numeric']  # 
  x_factor <- x %>% select(research_case_id, var_factor)
  x <- x %>% select(research_case_id, var)
  
  calculate_mode <- function(x) {
    uniqx <- unique(x)
    uniqx[which.max(tabulate(match(x, uniqx)))]
  }
  
  get_first <- function(y, na.rm = TRUE) {
    first <- y[which(!is.na(y))[1]]
    if (is.na(first)) {return (NA)}
    first
  }
  
  get_last <- function(y, na.rm = TRUE) {
    if (length(which(!is.na(y))) > 0) {
      last <- y[which(!is.na(y))[length(which(!is.na(y)))]]
      return(last)
    }    
    return(NA)
  }
  
  my_sd <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {return(NA)}
    if (sum(!is.na(x))==1) {return(0)}
    else return(sd(x, na.rm = TRUE))
  }
  
  my_skewness <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {return(NA)}
    if (sum(!is.na(x))==1) {return(0)}
    else return(skewness(x, na.rm = TRUE))
  }
  
  my_kurtosis <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {return(NA)}
    if (sum(!is.na(x))==1) {return(0)}
    else return(kurtosis(x, na.rm = TRUE))
  }
  
  factors_agg <- x_factor %>%
    group_by(research_case_id) %>% 
    summarise_at(var_factor, funs(mode = calculate_mode(.),
                                  n = sum(!is.na(.)), first = get_first(.), 
                                  last = get_last(.)))
  #names(factors_agg)[2:ncol(factors_agg)] <- sapply(var_factor, function(y) {sapply(names(factors_agg)[2:5], 
  #                                                                                  function(x) {paste0(y, "_",x)})})
  aggregated <-  x %>%
    group_by(research_case_id) %>% 
    summarise_at(var, funs(max, min, median, mean, sd = my_sd(.), 
                           n = sum(!is.na(.)), first = get_first(.), last = get_last(.),
                           lq = quantile(., prob = 0.25), uq = quantile(., prob = 0.75),
                           skew = my_skewness(.), kurtosis = my_kurtosis(.)), 
                 na.rm = TRUE) %>% left_join(factors_agg, by = 'research_case_id')
  
  aggregated 
}
# currently: if no info, gives - inf as max, inf as min and NA or NaN for the other values
library(dplyr)
library(readr)
library(lubridate)
library(e1071)
# set working directory to data folder - Contains all: A-S cleaned data files, as per 09.05.2021
setwd("~/Desktop/Data Folder - StatsLab - May 21")

# needed for operations data
load('H_Operationen_Surgery_Times_long.RData')
flags <- read_csv('T_Outcome_Y_Coded.csv')
died_wo_surgery <- (flags %>% filter(has_operation==0) %>% filter(Death==1) %>% select(research_case_id))$research_case_id


# bga ---------
bga <- read.csv("A_Bga_Werte.csv", stringsAsFactors = T) %>% mutate(Messung_dttm = ymd_hms(Messung_dttm, tz = "Europe/Zurich"))
bga.vars <- c("Lac", "T", "pO2", "FIO2")
bga_agg <- get_statistics(bga, var = bga.vars, time_var = 'Messung_dttm') 
# those who died without surgery: 29 of them
bga_died_agg <- get_statistics_died(bga %>% filter(research_case_id %in% died_wo_surgery), bga.vars)

bga_final <- bind_rows(bga_agg, bga_died_agg)

save(bga_final, file = "bga_aggregate.RData")


# blut:------
blut <- read.csv("B_Blut_Werte_Categorized.csv", stringsAsFactors = T) %>% mutate(ObservationDtTm = ymd_hms(ObservationDtTm, tz = "Europe/Zurich"))
blut.vars <- c("CKDEPI", "BAS", "BASA", "CRP", "IMGR", "IMGRA", 
                           "LC", "LYM", "NEU", "NEUA", "PCT", "PCTB", "TC", 
                           "FBG", "GGT", "KHINR", "PTZEIT", "QUICK", "TZI", 
                           "HUFH", "CK", "LDH", "MYO", "CKDEPI_Categ", 
               "HB", "KREA", "MYO_Categ") # also add the categories?
blut_agg <- get_statistics(blut, var = blut.vars, time_var = 'ObservationDtTm') 
blut_died_agg <- get_statistics_died(blut %>% filter(research_case_id %in% died_wo_surgery), blut.vars)
blut_final <- bind_rows(blut_agg, blut_died_agg)
save(blut_final, file = "blut_aggregate.RData")


# vitals: ------
vitals <- read.csv("R_Vital_Werte.csv", stringsAsFactors = T) %>% mutate(STARTDAT = ymd_hms(STARTDAT, tz = "Europe/Zurich"))
vitals.vars <- names(vitals)[4:8]
vitals_agg <- get_statistics(vitals, var = vitals.vars, time_var = 'STARTDAT') 
vitals_died_agg <- get_statistics_died(vitals %>% filter(research_case_id %in% died_wo_surgery), vitals.vars)
vitals_final <- bind_rows(vitals_agg, vitals_died_agg)
save(vitals_final, file = "vitals_aggregate.RData")

