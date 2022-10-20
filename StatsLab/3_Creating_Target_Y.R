library('tibble')
library('dplyr')
library('ggplot2')
library('lubridate')
library('tidyr')

raw_surgeries <- read.csv('data/raw_data/DCO_Defsurg.csv', stringsAsFactors = T)
surgeries_new <- read.csv('../surgeries_new_processed_on_raw.csv', stringsAsFactors = T)
death <- read.csv('data/tidy_data/I_Patient_Unfall.csv', stringsAsFactors = T)
mapping <- read.csv('data/tidy_data/E_Mapping_AIS.csv', stringsAsFactors = T)
load('data/tidy_data/operations')
surgeries_old <- operations

# select death outcome and research case id only
death <- death %>%
  select(research_case_id, Death)

case_id <- mapping %>%
  select(research_case_id) %>%
  unique() %>%
  unlist()

filter_dataset <- function(data, ids){
  filtered <- data %>%
    filter(research_case_id %in% ids) %>%
    as_tibble()
  
  return(filtered)
}

join_data <- function(d1, d2){
  merged <- d1 %>%
    left_join(d2, by = c('research_case_id')) %>%
    as_tibble()
  
  return(merged)
}

# mapping keep only relevant columns
mapping <- mapping %>% select(research_case_id) %>% distinct()
mapping['has_operation'] <- 0

# death - keep only AIS >= 3
death <- filter_dataset(death, case_id)

# keep only relevant columns for operations old dataset
surgeries_old <- surgeries_old %>% ungroup() %>% select(research_case_id, Startdat, Stopdat)

# transform to datetime
surgeries_old <- surgeries_old %>%
  mutate(Stopdat = as.POSIXlt(Stopdat, format = "%Y-%m-%d %H:%M:%S"),
         Startdat = as.POSIXlt(Startdat, format = "%Y-%m-%d %H:%M:%S"))

surgeries_new <- surgeries_new %>%
  mutate(Therapy_dat = as.POSIXlt(Therapy_dat, format = "%Y-%m-%d %H:%M:%S"))

new_processed_patients <- data.frame()

for(patient in unique(surgeries_new$research_case_id)){
  subset_new <- subset(surgeries_new, research_case_id == patient)
  subset_old <- subset(surgeries_old, research_case_id == patient)
  list_dates <- unique(subset_new$Therapy_dat)
  
  new_processed <- data.frame()
  for(tdt in 1:length(list_dates)){
    if(nrow(subset_old) != 0){
      subset_dates_old <- subset(subset_old, Stopdat >= list_dates[tdt])
      if(nrow(subset_dates_old) != 0){
        subset_dates_old <- subset_dates_old %>% mutate(time_diff = Stopdat - list_dates[tdt]) %>% arrange(., time_diff)
        one_line_new <- subset_new[subset_new$Therapy_dat == list_dates[tdt],]
        one_line_new$Stopdat <- subset_dates_old$Stopdat[[1]]
        new_processed <- bind_rows(new_processed, one_line_new)
      }else{
        one_line_new <- subset_new[subset_new$Therapy_dat == list_dates[tdt],]
        one_line_new$Stopdat <- NA
        new_processed <- bind_rows(new_processed, one_line_new)
        
      }
    }else{
      one_line_new <- subset_new[subset_new$Therapy_dat == list_dates[tdt],]
      one_line_new$Stopdat <- NA
      new_processed <- bind_rows(new_processed, one_line_new)
    }

  }
  new_processed_patients <- bind_rows(new_processed_patients, new_processed)
}

surgeries_old_augmented <- data.frame()
for(patient in unique(surgeries_old$research_case_id)){
  subset_new <- subset(new_processed_patients, research_case_id == patient)
  subset_old <- subset(surgeries_old, research_case_id == patient)
  
  for(item in c(1:nrow(subset_old))){
    subset_old[item, 'surgery_type'] <- ""
    subset_old[item, 'hemo_is_involved'] <- NA
    
    if(nrow(subset_new) != 0){
      sub_subset_new <- subset(subset_new, Stopdat == subset_old$Stopdat[[item]])
      if(nrow(sub_subset_new) != 0){
        date_new <- unique(sub_subset_new$Stopdat)
        subset_old[item, 'surgery_type'] <- as.character(sub_subset_new$Intervention)[nrow(sub_subset_new)]
        subset_old[item, 'hemo_is_involved'] <- sub_subset_new$hemo_is_involved[nrow(sub_subset_new)]
      }
    }  
  }
  surgeries_old_augmented <- bind_rows(surgeries_old_augmented, subset_old)
}

new_processed_patients %>%
  filter(!is.na(Stopdat))%>%
  group_by(research_case_id, Stopdat) %>%
  summarise(n = n()) %>%
  filter(n!= 1)

mapping <- join_data(mapping, death)

list_patient_with_operation <- unique(surgeries_old_augmented$research_case_id)

mapping[mapping$research_case_id %in% list_patient_with_operation, 'has_operation'] <- as.numeric(1) 

mapping <- join_data(mapping, surgeries_old_augmented)

mapping[, 'flag'] <- NA

mapping$flag[which(mapping$Death == 1 & mapping$has_operation == 0 & is.na(mapping$flag))] <- 0
mapping$flag[which(mapping$Death == 0 & mapping$has_operation == 0 & is.na(mapping$flag))] <- 'remove'
mapping$flag[which(mapping$has_operation == 1 & mapping$surgery_type == "" & is.na(mapping$flag))] <- 'unknown operation type'
mapping$flag[which(mapping$has_operation == 1 & mapping$surgery_type == "DCO" & is.na(mapping$flag))] <- 0
mapping$flag[which(mapping$has_operation == 1 & mapping$surgery_type == "DefSurg" & mapping$hemo_is_involved == 1 & is.na(mapping$flag))] <- 0

def_surgeries_count <- surgeries_old_augmented %>% filter(surgery_type == 'DefSurg', hemo_is_involved == 0) %>% 
  select(research_case_id, surgery_type) %>%
  group_by(research_case_id, surgery_type) %>%
  summarise(n = n())

mapping['multiple'] <- 0
for (patient in unique(def_surgeries_count$research_case_id)){
  map_subset <- subset(mapping, research_case_id == patient)
  if(nrow(map_subset) != 0){
    mapping[which(mapping$research_case_id == patient & mapping$surgery_type == 'DefSurg' & mapping$hemo_is_involved == 0), 'multiple'] <- def_surgeries_count[def_surgeries_count$research_case_id == patient, 'n']
  }
}

mapping[which(mapping$has_operation == 1 & mapping$surgery_type == "DefSurg" &
                mapping$hemo_is_involved == 0 & mapping$multiple == 1 & mapping$Death == 1 & is.na(mapping$flag)), 'flag'] <- as.character(0)
mapping[which(mapping$has_operation == 1 & mapping$surgery_type == "DefSurg" &
                mapping$hemo_is_involved == 0 & mapping$multiple == 1 & mapping$Death == 0 & is.na(mapping$flag)), 'flag'] <- 'check vitals'

is_last_definitive <- mapping %>% filter(surgery_type == 'DefSurg', hemo_is_involved == 0, multiple > 1)

ordered_multiple_surgeries <- is_last_definitive %>%
  group_by(research_case_id) %>%
  arrange(., Stopdat) %>%
  mutate(surgery_order = row_number())

multiple_flagged <- data.frame()
for(patient in unique(ordered_multiple_surgeries$research_case_id)){
  subset_definitive <- subset(ordered_multiple_surgeries, research_case_id == patient)
  subset_definitive['is_last'] <- 0
  subset_definitive[which(subset_definitive$surgery_order == nrow(subset_definitive)), 'is_last'] <- 1 
  multiple_flagged <- bind_rows(multiple_flagged, subset_definitive)
}

mapping['is_last'] <- 0

multiple_flagged_filtered <- multiple_flagged %>% filter(is_last == 1)

for(patient in unique(multiple_flagged_filtered$research_case_id)){
  stopdat <- multiple_flagged_filtered[which(multiple_flagged_filtered$research_case_id == patient), 'Stopdat']
  mapping[which(mapping$research_case_id == patient & mapping$Stopdat == stopdat & mapping$surgery_type == 'DefSurg'), 'is_last'] <- multiple_flagged_filtered[which(multiple_flagged_filtered$research_case_id == patient), 'is_last']
}

mapping$flag[which(mapping$has_operation == 1 & mapping$surgery_type == "DefSurg" & mapping$hemo_is_involved == 0 & is.na(mapping$flag) &
                     mapping$multiple > 1 & mapping$is_last == 1 & mapping$Death == 1)] <- 0

mapping$flag[which(mapping$has_operation == 1 & mapping$surgery_type == "DefSurg" & mapping$hemo_is_involved == 0 & is.na(mapping$flag) &
                     mapping$multiple > 1 & mapping$is_last == 1 & mapping$Death == 0)] <- 'check vitals'

mapping$flag[which(mapping$has_operation == 1 & mapping$surgery_type == "DefSurg" & mapping$hemo_is_involved == 0 & is.na(mapping$flag) &
                     mapping$multiple > 1 & mapping$is_last == 0)] <- 'check vitals'

mapping %>% filter(flag != 'unknown operation type', flag != 'remove') %>% dim()

write.csv(mapping, 'T_Outcome_Y_Coded_newaggr.csv', row.names = F)

old <- read.csv('../T_Outcome_Y_Coded_new.csv')

old %>% filter(flag != 'unknown operation type', flag != 'remove') %>% dim()

coded_surgeries_old <- coded_surgeries_old %>% distinct()

test <- surgeries_old %>% inner_join(surgeries_new, by ='research_case_id')

test %>% select(research_case_id) %>% distinct() %>% dim()

surgeries_old %>% select(research_case_id) %>% distinct() %>% dim()
