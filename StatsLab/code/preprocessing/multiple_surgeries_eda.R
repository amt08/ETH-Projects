# EDA for multiple surgeries. How many AIS’s do they have, how many damage control, how many definitive.

library('tibble')
library('dplyr')
library('ggplot2')
library('visdat')
library('lubridate')
library('tidyr')


mapping <- read.csv('data/tidy_data/Mapping (all trauma).csv',
                    stringsAsFactors = T)

operations <- read.csv('data/raw_data/[SDS].[v_Export_Operationen_Proc_tra_sds].csv',
                       stringsAsFactors = T)

hospital <- read.csv('data/raw_data/[SDS].[v_Export_Patient_Unfall_SpitalAufenthalt_tra_sds].csv',
                     stringsAsFactors = T)

mapping %>%
  select(research_case_id) %>%
  unique() %>%
  dim()

# get relevant case ids

case_id <- mapping %>%
  select(research_case_id) %>%
  unique() %>%
  unlist()

# filter on relevant case ids

filter_dataset <- function(data, ids){
  
  filtered <- data %>%
    filter(research_case_id %in% ids) %>%
    as_tibble()
  
  return(filtered)
  
}

op <- filter_dataset(operations, case_id)
hosp <- filter_dataset(hospital, case_id)

# all empty strings changed to NA

fill_na <- function(dat){
  dat <- dat %>% mutate_all(list(~na_if(., "")))
  return(dat)
}

op <- fill_na(op)
hosp <- fill_na(hosp)

# join datasets on research_case_id and research_id

join_data <- function(d1, d2){
  merged <- d1 %>%
    left_join(d2, by = c('research_id', 'research_case_id')) %>%
    as_tibble()
  
  return(merged)
}

op_hosp <- join_data(op, hosp)

# create death variable
op_hosp <- op_hosp %>%
  mutate(death = ifelse(is.na(Tod_Eingetroffen_Datumzeit), 0, 1), .keep = 'all')

# filter by patients with multiple surgeries

multiple_ids <- op_hosp %>% select(research_case_id, Startdat) %>%
  group_by(research_case_id) %>% summarise(n = n()) %>%
  filter(n >= 2) %>% select(research_case_id) %>%
  unlist()


multiple_surg_mapping <- filter_dataset(mapping, multiple_ids)

# How many injuries have on average patients with multiple surgeries ?

multiple_surg_mapping %>%
  group_by(research_case_id) %>%
  summarise(injuries = n_distinct(ISS_BODY_REGION)) %>%
  ungroup() %>%
  summarise(total_injuries = sum(injuries),
            n = n_distinct(research_case_id),
            avg_no_injuries = total_injuries / n)

proportions <- multiple_surg_mapping %>%
  group_by(ISS_BODY_REGION) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  summarise(
    ISS_BODY_REGION = ISS_BODY_REGION,
    total=count / sum(count)) 

ggplot(proportions, aes(as.factor(ISS_BODY_REGION), total)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent of total ISS") + xlab('ISS Body Region') +
  theme_bw()

multiple_surg_mapping %>%
  group_by(MAX_AIS_SEVERITY) %>%
  summarise(count = n()) %>%
  ggplot(., aes(as.factor(MAX_AIS_SEVERITY), count)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  xlab('AIS Score') +
  theme_bw()
  
data <- join_data(multiple_surg_mapping, op_hosp) %>% select(colnames(op_hosp)) %>% unique()

data <- data %>%
  mutate(Beginnlagerungdttm = ymd_hms(Beginnlagerungdttm, tz = "Europe/Zurich"),
         Nahtdttm = ymd_hms(Nahtdttm, tz = "Europe/Zurich"),
         Startdat = ymd_hms(Startdat, tz = "Europe/Zurich"),
         Stopdat = ymd_hms(Stopdat, tz = "Europe/Zurich"),
         Saaldttm = ymd_hms(Saaldttm, tz = "Europe/Zurich"),
         ï..abgabedttm = ymd_hms(ï..abgabedttm, tz = "Europe/Zurich"),
         ausleitendedttm = ymd_hms(ausleitendedttm, tz = "Europe/Zurich"),
         Aussaaldttm = ymd_hms(Aussaaldttm, tz = "Europe/Zurich"),
         Einleitbeginndttm = ymd_hms(Einleitbeginndttm, tz = "Europe/Zurich"),
         Einleitendedttm = ymd_hms(Einleitendedttm, tz = "Europe/Zurich"),
         Eintreffeneinleitungdttm = ymd_hms(Eintreffeneinleitungdttm, tz = "Europe/Zurich"),
         Endeabdeckdttm = ymd_hms(Endeabdeckdttm, tz = "Europe/Zurich"),
         Opendedttm = ymd_hms(Opendedttm, tz = "Europe/Zurich"),
         Erstechirmassdttm = ymd_hms(Erstechirmassdttm, tz = "Europe/Zurich"))

data <- data %>% mutate(Delta = as.integer(difftime(Nahtdttm, Beginnlagerungdttm, units = 'mins')))

data <- data %>%
  mutate(surgery = ifelse(!is.na(Delta), ifelse(Delta >= 60, 'ETC', 'DCO'),
                             ifelse(!is.na(Totalortime), ifelse(Totalortime >= 45, 'ETC', 'DCO'),
                                    ifelse(!is.na(Saalbelegungzeit), ifelse(Saalbelegungzeit >= 45, 'ETC', 'DCO'), NA))))

data %>%
  group_by(surgery) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  summarise(surgery = surgery,
            percent = n / sum(n)) %>%
  ggplot(., aes(as.factor(surgery), percent)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent of total operations") + xlab('Surgery type') +
  theme_bw()

data <- data %>% mutate(surgery_v2 = ifelse(!is.na(Delta), ifelse(Delta >= 60, 'ETC', 'DC'),
                                                  ifelse(!is.na(Totalortime), ifelse(Totalortime >= 45, 'ETC', 'DC'), NA)))

data %>%
  group_by(surgery_v2) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  summarise(surgery = surgery_v2,
            percent = n / sum(n)) %>%
  ggplot(., aes(as.factor(surgery), percent)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  scale_y_continuous(labels=scales::percent) +
  ylab("Percent of total operations") + xlab('Surgery type') +
  theme_bw()

data %>%
  group_by(research_case_id) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  summarise(total = sum(n),
            patients = n_distinct(research_case_id))


multiple_surg_mapping %>%
  group_by(research_case_id) %>%
  summarise(ais_severity = n_distinct(MAX_AIS_SEVERITY)) %>%
  filter(ais_severity > 1)
