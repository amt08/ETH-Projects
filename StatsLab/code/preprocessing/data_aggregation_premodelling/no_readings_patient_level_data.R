library('renv')
library('tibble')
library('dplyr')
library('ggplot2')
library('visdat')
library('naniar')
library('lubridate')
library('readxl')
library('tidyr')

diagnosis <- read.csv('../Data/Stats Lab Cleaned Data/Diagnosen.csv', stringsAsFactors = T) # not really relevant I believe
mapping <- read.csv('../Data/Stats Lab Cleaned Data/Mapping_AIS.csv', stringsAsFactors = T)
complications_cleaned <- read.csv('../Data/Stats Lab Cleaned Data/Komplikationen.csv', stringsAsFactors = T) # whether patient had any complications or not
complications <- read.csv('../Data/raw/[SDS].[v_Export_Komplikationen_tra_sds].csv', stringsAsFactors = T)
pdms <- read.csv('../Data/Stats Lab Cleaned Data/PDMS_Mech_Beatmung.csv', stringsAsFactors = T)
iss <- read.csv('../Data/Stats Lab Cleaned Data/Score_ISS_from_AIS.csv', stringsAsFactors = T)
interventions <- read.csv('../Data/Stats Lab Cleaned Data/Untersuchungen.csv', stringsAsFactors = T)
patient_unfall <- read.csv('../Data/Stats Lab Cleaned Data/Patient_Unfall.csv', stringsAsFactors = T)

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

####################################################################
########## Complications - Sorted, just need to filter on mapping?

### complications is already one hot encoded but seems to have 2-3 rows per patient instead of 1
# going back to the raw file to output 1 row per patient
complic <- complications %>% select(ï..Code, research_case_id)
complic %>% pivot_wider(names_from = ï..Code, values_fill = 0)
complications_one_hot <- complic %>% mutate(value = 1) %>% 
  rename(complication = ï..Code) %>%
  spread(complication, value,  fill = 0, sep = "_")

#########################################################################

####################
#### Patient unfall

patient <- patient_unfall %>%
  rename(ASA_before_accident = Asa_vor_unfall,
         duration_hospital_stay_days = Dauer_Krankenhaus_Aufenthalt_Tagen,
         gender = Geschlecht,
         primary_or_assignment = Primaer_oder_zuweisung,
         age = Patient_alter,
         pregnancy = Schwangerschaft,
         death_after_hours = Tod_Nach_Stunden,
         trauma_mechanism = Trauma_mechanismus) %>%
  mutate(ASA_before_accident = ifelse(ASA_before_accident == 'I=gesund', 'ASA_healthy',
                                      ifelse(ASA_before_accident == 'II=leichte Einschränkung', 'ASA_slight_restriction',
                                             ifelse(ASA_before_accident == 'III=schwere systemische Einschränkung', 'ASA_sever_restriction',
                                                    ifelse(ASA_before_accident == 'IV=lebensbedrohliche Allgemeinerkrankung',
                                                           'ASA_life_threatening_illness', 'ASA_unknown'))))) %>%
  mutate(value = 1) %>% spread(ASA_before_accident, value,  fill = 0) %>%
  mutate(value = 1) %>% spread(gender, value,  fill = 0, sep = '_') %>%
  select(-gender_W) %>%
  mutate(value = 1) %>% spread(primary_or_assignment, value,  fill = 0, sep = '_') %>%
  mutate(age = ifelse(age == '79+', 80, as.numeric(age))) %>% # trick to create the bins
  mutate(age = cut(age, breaks=c(0, 19, 39, 59, 79, Inf), labels=c("0-19", "20-39", "40-59", "60-79", "80+"))) %>%
  mutate(value = 1) %>% spread(pregnancy, value,  fill = 0, sep = '_') %>%
  select(-c(pregnancy_Ja, Tod_Eingetroffen_Datumzeit)) %>%
  mutate(value = 1) %>% spread(age, value,  fill = 0, sep = '_') %>%
  mutate(value = 1) %>% spread(trauma_mechanism, value,  fill = 0, sep = '_')

#####################
### PDMS - Mechanical breathing
# in case of NA, mechanical breathing is 0

pdms <- pdms %>% mutate(ï..MetaVision_Mechanische_Beatmung_Dauer_min = ifelse(is.na(ï..MetaVision_Mechanische_Beatmung_Dauer_min), 0, ï..MetaVision_Mechanische_Beatmung_Dauer_min))

############################################################################
##### ISS should be kept as it is since the numbers are relevant for the degree of injury
### only need to filter on mapping

iss <- iss %>% rename(iss_score = ï..ISS)

############### what is Stat teilstat faelle ?


################ ISS body region - where the patient is injured one-hot encoded
### they get a new research case id I imagine if they return to the hospital, right?

iss_body_region <- mapping %>%
  select(research_case_id, ISS_BODY_REGION) %>%
  distinct() %>%
  mutate(value = 1, ISS_BODY_REGION = paste('ISS_', ISS_BODY_REGION)) %>% spread(ISS_BODY_REGION, value,  fill = 0)


mapping %>%
  select(research_case_id) %>%
  distinct() %>%
  summarise(n = n())

################### aggregating everything

patient <- filter_dataset(patient, case_id)
complication_encoded <- filter_dataset(complications_one_hot, case_id)
pdms <- filter_dataset(pdms, case_id) %>% select(-research_id) %>% rename(mechanical_breathing_mins = `ï..MetaVision_Mechanische_Beatmung_Dauer_min`)
iss <- filter_dataset(iss, case_id) %>% select(-research_id) # iss score
iss_body_region <- filter_dataset(iss_body_region, case_id)

first <- join_data(patient, complication_encoded) %>%
  mutate_at(vars(-c(colnames(patient))), replace_na, 0) %>% # assume now complications is na in left join
  join_data(., pdms) %>%
  mutate_at(vars(mechanical_breathing_mins), replace_na, 0) %>%
  join_data(., iss) %>%
  join_data(., iss_body_region)

##########################################################################################################################
##################################################################################### file sent by Sascha - Data Filtering
######################################################################################

get_operations_data <- function(path){
  
  labeled <- read.csv(path, stringsAsFactors = T)
  target <- labeled %>% filter(HighInterest == 1)
  
  target %>%
    group_by(research_case_id) %>%
    summarise(n = n()) %>%
    filter(n != 1)
  
  mask <- target %>% duplicated()
  
  return(target[!mask, ])
  
}


######################################### This should be the final version of the file before matching with what we already had
all_operations <- get_operations_data('data/raw_data/DCO_Defsurg.csv')


operations <- read.csv('../../Data/Stats Lab Cleaned Data/Operationen.csv', stringsAsFactors = T)


dedup %>% select(research_case_id) %>%
  distinct() %>%
  summarise(n=n()) # 6881 of research case id's of interest

all_cases <- read.csv('../../Data/[SDS].[v_Export_Mapping_Diagnosen_ICD_to_AIS_tra_sds].csv', stringsAsFactors = T)

data_remaining <- all_cases %>% inner_join(dedup, by ='research_case_id')


data_remaining %>%
  select(research_case_id) %>%
  distinct() %>%
  summarise(n=n()) # 6343 of research case ids


############################################################################
############### Number of surgeries for each patient

only_operations <- all_operations %>% filter(Intervention %in% c('DefSurg', 'DCO')) %>%
  select(research_case_id, Intervention, Therapy_dat)

# what is the average time between surgeries for a patient with multiple surgeries

multiple_operations_ids <- only_operations %>%
  group_by(research_case_id) %>%
  summarise(n = n()) %>%
  filter(n != 1) %>%
  ungroup() %>%
  select(research_case_id)

multiple_operations <- filter_dataset(only_operations, multiple_operations_ids$research_case_id)

