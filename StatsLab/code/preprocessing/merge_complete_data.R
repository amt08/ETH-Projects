# Aim: To merge all aggregated readings, single readings and y details....
# aggregations: blut, bga and wert
# single: demographics
# y: binary
library(dplyr)
library(readr)
library(visdat)
library(lubridate)
setwd("~/Desktop/Data Folder - StatsLab - May 21")

# data needed -------
load('V_Outcome_Y_Coded.RData')
flags <- patients
flags <- flags %>% mutate(Startdat = ymd_hms(Startdat))
rm(patients)
flags <- flags %>% group_by(research_case_id) %>% arrange(Startdat) %>% 
  mutate(SurgeryNr = ifelse(has_operation==1, row_number(), NA))

# Look at those with a Y!
# y = flag
labelled <- flags %>% filter(flag == 0 | flag == 1)

# aggregated data load: COMPLETE ------
load('vitals_aggregate_complete.RData')
load('bga_aggregate_complete.RData')
load('blut_aggregate_complete.RData')

# need to join by start time as well as case id...
# none in blut is fully full?
all_aggregated <- merge(vitals_final, bga_final, by = c('research_case_id', 'SurgeryNr'), all.x = T, all.y = T)
all_aggregated[sapply(all_aggregated, is.infinite)] <- NA
# 7193 for which we have aggregated readings.

df_full <- left_join(labelled, all_aggregated, c('research_case_id', 'SurgeryNr'))

# first we look at rows where ALL vitals are missing:

# where all vitals are missing:
missing_rows <- apply(df_full, 1, function(x) sum(is.na(x)))

df_full <- df_full %>% bind_cols(missing = missing_rows)

df_full %>% filter(missing==ncol(df_full)-14) # 2,389 with basically no data
df_full %>% filter(missing==ncol(df_full)-14) %>% filter(SurgeryNr==1) # 1,861 of these are first surgeries!
table((df_full %>% filter(missing==ncol(df_full)-14) %>% 
         filter(SurgeryNr==1) %>% select(surgery_type))[['surgery_type']]) 
# 1243 are definitive. A lot of first surgeries don't have any vitals!
# Let's get rid of all without any vitals:

# we have: 5143 at the moment
df_no_empty <- df_full %>% filter(missing!=ncol(df_full)-14)
# down to: 4280

# now we have: 
n <- nrow(df_no_empty)

hist(sapply(df_no_empty, function(x) {sum(is.na(x))/n}), xlab = "Missing percent", 
     main = "Number of variables with their missing percent")

all_var_names <- names(df_no_empty)[grep("mean", names(df_no_empty))]

vis_dat(df_no_empty[ , all_var_names])


# mostly fine. SD = NA when n = 1, as do kurtosis and skewness, so that's why!

single_meas <- read.csv("U_Single_Measurements_Patient.csv")




