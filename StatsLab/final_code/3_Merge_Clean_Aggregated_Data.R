# 3_Merge_clean_aggregated_data: merges, cleans and processes the aggregated data
# libraries
library(dplyr)
library(readr)
library(visdat)
library(lubridate)
library(tidyverse)
library(data.table)
library(imputeTS)

setwd("~/Desktop/Data Folder - StatsLab - May 21")

# data needed -------
load('V_Outcome_Y_Coded.RData')
flags <- patients
flags <- flags %>% mutate(Startdat = ymd_hms(Startdat, tz = "Europe/Zurich"),
                          Stopdat = ymd_hms(Stopdat, tz = 'Europe/Zurich'))
rm(patients)
flags <- flags %>% group_by(research_case_id) %>% arrange(Startdat) %>% 
  mutate(SurgeryNr = ifelse(has_operation==1, row_number(), NA))


# aggregated data load ------
load('vitals_aggregate.RData')
load('bga_aggregate.RData')
load('blut_aggregate.RData')

# need to join by start time as well as case id...

all_aggregated <- merge(vitals_final, bga_final, by = c('research_case_id', 'SurgeryNr'), all.x = T, all.y = T)
all_aggregated <- merge(all_aggregated, blut_final, by = c('research_case_id', 'SurgeryNr'), all.x = T, all.y = T)
all_aggregated[sapply(all_aggregated, is.infinite)] <- NA
# 5913 for which we have aggregated readings.


df_full <- merge(flags, all_aggregated, c('research_case_id', 'SurgeryNr'))

n <- nrow(df_full)
missing_percent <- sapply(df_full, function(x) {sum(is.na(x))/n})
all_var_names <- names(df_full)[grep("first", names(df_full))][1:34]
# visualise
vis_miss(df_full[ , all_var_names])
sum(complete.cases(as.matrix(df_full[, -c(1:13, ncol(df_full))]))) # 0


# FACTORISE ------
missing_threshold <- 0.55
names_factor <-  names(df_full)[missing_percent >= missing_threshold]
names_factor <- names_factor[grep("first", names_factor)] # don't want kurtosis, skewness or sd (NA when n=1)
str_rem <- 'PCT|PCTB|PTZEIT|HUFH|LYM|NEU|LDH|GGT|IMGR|IMGRA|MYO|TZI|GCS'
# PCT, PCTB, PTZEIT, HUFH - 4 tests to convert to binary!
to_remove <- names(df_full)[grep(str_rem, names(df_full))]

df_full_factorised <- df_full %>% 
  mutate(PCTB = ifelse(is.na(PCTB_first), 0, 1)) %>%
  mutate(PTZEIT = ifelse(is.na(PTZEIT_first), 0, 1)) %>%
  mutate(HUFH = ifelse(is.na(HUFH_first), 0, 1)) %>%
  mutate(PCT = ifelse(is.na(PCT_first), 0, 1)) %>%
  mutate(LYM = ifelse(is.na(LYM_first), 0, 1)) %>%
  mutate(NEU = ifelse(is.na(NEU_first), 0, 1)) %>%
  mutate(LDH = ifelse(is.na(LDH_first), 0, 1)) %>%
  mutate(GGT = ifelse(is.na(GGT_first), 0, 1)) %>%
  mutate(IMGR = ifelse(is.na(IMGR_first), 0, 1)) %>%
  mutate(IMGRA = ifelse(is.na(IMGRA_first), 0, 1)) %>%
  mutate(MYO = ifelse(is.na(MYO_first), 0, 1)) %>%
  mutate(TZI = ifelse(is.na(TZI_first), 0, 1)) %>%
  mutate(GCS = GCS_mean) %>%
  dplyr::select(-to_remove)

sum(complete.cases(as.matrix(df_full_factorised[, -c(1:13, ncol(df_full_factorised))]))) # 87
# there are still some discrepancies to fix

# n = NA, make it 0. 
df_full_factorised[, n_var][is.na(df_full_factorised[, n_var])] <- 0
# remove DtTmStop and DtTmStart
# replace any NaNs with NAs (exist in a few columns, e.g. kurtsosi)
df_full_factorised <- df_full_factorised %>% select(-c(DtTmStop,DtTmStart)) %>% ungroup() %>%
  mutate_all(~replace(., is.nan(.), NA))


# T and temperatur: the same. Merge.
to_remove <- names(df_full_factorised)[grep("_Categ", names(df_full_factorised))]
t <- names(df_full_factorised)[grep("T_", names(df_full_factorised))]

df_full_factorised <- df_full_factorised %>% 
  mutate(Temperatur_max = coalesce(Temperatur_max, T_max)) %>%
  mutate(Temperatur_min = coalesce(Temperatur_min, T_min)) %>%
  mutate(Temperatur_median = coalesce(Temperatur_median, T_median)) %>%
  mutate(Temperatur_mean = coalesce(Temperatur_mean, T_mean)) %>%
  mutate(Temperatur_sd = coalesce(Temperatur_sd, T_sd)) %>%
  mutate(Temperatur_n = coalesce(Temperatur_n, T_n)) %>%
  mutate(Temperatur_first = coalesce(Temperatur_first, T_first)) %>%
  mutate(Temperatur_last = coalesce(Temperatur_last, T_last)) %>%
  mutate(Temperatur_lq = coalesce(Temperatur_lq, T_lq)) %>%
  mutate(Temperatur_uq = coalesce(Temperatur_uq, T_uq)) %>%
  mutate(Temperatur_skew = coalesce(Temperatur_skew, T_skew)) %>%
  mutate(Temperatur_kurtosis = coalesce(Temperatur_kurtosis, T_kurtosis)) %>%
  dplyr::select(-t, -to_remove)

all_var_names <- names(df_full_factorised)[grep("first", names(df_full_factorised))]

vis_miss(df_full_factorised[ , all_var_names])
sum(complete.cases(as.matrix(df_full_factorised[, -c(1:11, ncol(df_full_factorised))]))) # 272


df <- df_full_factorised
save(df, file = 'X_Aggregated_Features.RData')


## Large-scale imputation
my_repo <- "~/Documents/ETH/Stats Lab/"
source(paste0(my_repo, "polytrauma/code/preprocessing/impute_hard.R"))
source(paste0(my_repo, "polytrauma/code/preprocessing/impute_soft.R"))

# Loading the imputation functions & normal values
#source("impute_hard.R")
#source("impute_soft.R")
load("Normal_values.RData")

# Preparing the data sets
load("U_Single_Measurements_Patient.RData")

icu_ids <- as.matrix(patients %>% filter(icu == 1) %>% select(research_case_id)) # research case IDs for those who went to the ICU
dead_without_surgery_ids <- as.matrix(df %>% filter(has_operation == 0) %>% select(research_case_id)) # research case IDs for those who died without surgery

# Splitting the data sets into 2: One for those who died without surgery & for the rest
df_surgery_rest <- df
df_no_surgery_dead <- df[c(), ]
for (i in dead_without_surgery_ids) {
  if (i %in% df$research_case_id) {
    df_no_surgery_dead <- rbind(df_no_surgery_dead, df_surgery_rest[df_surgery_rest$research_case_id == i, ])
    df_surgery_rest <- df_surgery_rest[df_surgery_rest$research_case_id != i, ]
  }
}


# Splitting the "df_surgery_rest" data set again in 2: One for those who went to the ICU and one for those who did not
df_no_icu <- df_surgery_rest
df_only_icu <- df_surgery_rest[c(), ]
for (i in icu_ids) {                          ## this step can take ~ 1-2 minutes
  if (i %in% df$research_case_id) {
    df_only_icu <- rbind(df_only_icu, df_no_icu[df_no_icu$research_case_id == i, ])
    df_no_icu <- df_no_icu[df_no_icu$research_case_id != i, ]
  }
}

# Which ones should be imputed
impute = "hard"
if (impute == "soft") {
  df_no_icu <- impute_soft(df_no_icu)         ## this step can take ~ 2-3 minutes
  df_soft <- rbind(df_no_icu, df_only_icu, df_no_surgery_dead)
  df_soft <- df_soft %>% group_by(research_case_id)
  df_soft <- df_soft[, -length(df_soft)]
  save(df_soft, file = 'df_soft.RData')
}

if (impute == "hard") {
  ## the entire process can take ~ 8-10 minutes
  df_no_icu <- impute_hard(df_no_icu)
  df_only_icu <- impute_hard(df_only_icu, ICU = TRUE)
  
  df_all_imputed <- rbind(df_no_icu, df_only_icu, df_no_surgery_dead)
  
  # Putting the remaining higher moments to 0
  for (j in names(df_all_imputed)) {
    if (j %in% names(df_all_imputed)[names(df_all_imputed) %like% "_n|_sd|_skew|_kurtosis"]) {
      df_all_imputed[is.na(df_all_imputed[, j]), j] <- 0
    }
  }
  
  df_all_imputed <- df_all_imputed[, -length(df_all_imputed)]
  df_all_imputed <- df_all_imputed %>% group_by(research_case_id)
  save(df_all_imputed, file = 'df_all_imputed_raw.RData')
}


# other vars to be added ------

load("df_all_imputed_raw.Rdata")
load("U_Single_Measurements_Patient.RData")

df <- df_all_imputed  %>% filter(flag == 1| flag == 0)

ais <- read.csv('E_Mapping_AIS.csv')

ais <- ais %>% group_by(research_case_id) %>% 
  mutate(head_injury = ifelse(1 %in% ISS_BODY_REGION, 1, 0)) %>%
  dplyr::select(-c(research_id, Code, Dat)) %>%
  summarise(max_ais = max(MAX_AIS_SEVERITY), 
            head_injury = sum(head_injury)) %>%
  mutate(head_injury = ifelse(head_injury > 0, 'Yes', 'No')) %>%
  ungroup
df <- df %>% left_join(ais, by = 'research_case_id')

var_to_remove <- c('Death', 'has_operation', 'multiple', 'is_last',
                   'hemo_is_involved', 'Startdat', 'Stopdat', 'surgery_type')
single_meas <- patients
library(dplyr)
df <- df %>% dplyr::select(-all_of(var_to_remove)) %>% 
  left_join(single_meas, by = 'research_case_id')
df$SurgeryNr[is.na(df$SurgeryNr)] <- 0
sapply(df, function(x) {sum(is.na(x))/length(x)})

vis_miss(df %>% dplyr::select(-matches("lq|uq|min|max|skew|mean|median|last|_n|sd|kurtosis"))) # all imputed

save(df, file =  'df_all_imputed.RData')

df <- df[complete.cases(df[, 6:ncol(df)]),]

save(df, file =  'df_all_imputed_complete_cases.RData')

# unimputed full cases
rm(list = ls())
load('X_Aggregated_features.RData') # without imputation
var_to_remove <- c('Death', 'has_operation', 'multiple', 'is_last', 'GCS',
                   'hemo_is_involved')
load("U_Single_Measurements_Patient.RData")
single_meas <- patients
df <- df %>% filter(flag == 1| flag == 0) %>% 
  dplyr::select(-all_of(var_to_remove)) %>% 
  left_join(single_meas, by = 'research_case_id')

ais <- read.csv('E_Mapping_AIS.csv')

ais <- ais %>% group_by(research_case_id) %>% 
  mutate(head_injury = ifelse(1 %in% ISS_BODY_REGION, 1, 0)) %>%
  dplyr::select(-c(research_id, Code, Dat)) %>%
  summarise(max_ais = max(MAX_AIS_SEVERITY), 
            head_injury = sum(head_injury)) %>%
  mutate(head_injury = ifelse(head_injury > 0, 'Yes', 'No')) %>%
  ungroup
df <- df %>% left_join(ais, by = 'research_case_id')

df$SurgeryNr[is.na(df$SurgeryNr)] <- 0
sapply(df, function(x) {sum(is.na(x))/length(x)})
features_drop <- c('Startdat', 'Stopdat', 'surgery_type',
                   names(df)[grep('kurtosis', names(df))])
df <- df %>% dplyr::select(-features_drop)


sum(complete.cases(df[, c(4:ncol(df))])) # 201!!!!

df_complete <- df[complete.cases(df[, 4:ncol(df)]) ,]

save(df_complete, file = 'unimputed_complete_cases.Rdata')



