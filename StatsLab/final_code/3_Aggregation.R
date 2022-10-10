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

