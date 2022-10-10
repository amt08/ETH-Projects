library('renv')
library('tibble')
library('dplyr')
library('ggplot2')
library('visdat')
library('naniar')
library('lubridate')


# no surgeries
# single surgeries

# reading in data
mapping <- read.csv('/Users/Taru/desktop/Dataset/Mapping (all trauma).csv',
                    stringsAsFactors = T) # only severe trauma here!!

operations <- read.csv('/Users/Taru/desktop/Dataset/[SDS].[v_Export_Operationen_Proc_tra_sds].csv',
                       stringsAsFactors = T)

hospital <- read.csv('/Users/Taru/desktop/Dataset/[SDS].[v_Export_Patient_Unfall_SpitalAufenthalt_tra_sds].csv',
                     stringsAsFactors = T)


# any missing values turn into NA
fill_na <- function(dat){
  dat <- dat %>% mutate_all(list(~na_if(., "")))
  return(dat)
}


mapping <- fill_na(mapping)
operations <- fill_na(operations)
hospital <- fill_na(hospital)

# list of relevant research case ids

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

# remove from the hospital and operations datasets the case ids we are not interested in

hospital_clean <- filter_dataset(hospital, case_id)
operations_clean <- filter_dataset(operations, case_id)

# join datasets on research_case_id and research_id

join_data <- function(d1, d2){
  merged <- d1 %>%
    left_join(d2, by = c('research_id', 'research_case_id')) %>%
    as_tibble()
  
  return(merged)
}

op_hosp <- join_data(operations_clean, hospital_clean) # already filtered by appropriate ais

# create death variable
op_hosp <- op_hosp %>%
  mutate(death = ifelse(is.na(Tod_Eingetroffen_Datumzeit), 0, 1), .keep = 'all')



op_hosp <- op_hosp %>%
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
         Erstechirmassdttm = ymd_hms(Erstechirmassdttm, tz = "Europe/Zurich"),
         Tod_Eingetroffen_Datumzeit = ymd_hms(Tod_Eingetroffen_Datumzeit, tz = "Europe/Zurich"),
         Fallende_DatumZeit = ymd_hms(Fallende_DatumZeit, tz = "Europe/Zurich"))

