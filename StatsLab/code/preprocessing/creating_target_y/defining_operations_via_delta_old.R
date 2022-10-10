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

# all empty strings changed to NA

fill_na <- function(dat){
  dat <- dat %>% mutate_all(list(~na_if(., "")))
  return(dat)
}

op <- fill_na(op)

# join datasets on research_case_id and research_id

join_data <- function(d1, d2){
  merged <- d1 %>%
    left_join(d2, by = c('research_id', 'research_case_id')) %>%
    as_tibble()
  
  return(merged)
}

###########################################################

### time-dependent variables

op <- op %>%
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

# create delta with what we have available

op <- op %>% mutate(Delta = as.integer(difftime(Nahtdttm, Beginnlagerungdttm, units = 'mins')))


################################################################################################################################

#####creating the definitive surgery vs. damage control - version 1, using Delta and Stopdat-Startdat

op <- op %>% mutate(Start_delta = as.integer(difftime(Stopdat, Startdat, units = 'mins')))

op <- op %>% mutate(surgery_v1 = ifelse(!is.na(Delta), ifelse(Delta >= 60, 'ETC', 'DC'),
                                                  ifelse(!is.na(Start_delta), ifelse(Start_delta >= 50, 'ETC', 'DC'), NA)))

#####creating the definitive surgery vs. damage control - version 2, using Delta and TotalORtime

op <- op %>% mutate(surgery_v2 = ifelse(!is.na(Delta), ifelse(Delta >= 60, 'ETC', 'DC'),
                                                  ifelse(!is.na(Totalortime), ifelse(Totalortime >= 45, 'ETC', 'DC'), NA)))


#####creating the definitive surgery vs. damage control - version 3, using Delta, TotalORtime and Saalbelegungzeit

op <- op %>%
  mutate(surgery_v3 = ifelse(!is.na(Delta), ifelse(Delta >= 60, 'ETC', 'DC'),
                             ifelse(!is.na(Totalortime), ifelse(Totalortime >= 45, 'ETC', 'DC'),
                                    ifelse(!is.na(Saalbelegungzeit), ifelse(Saalbelegungzeit >= 45, 'ETC', 'DC'), NA))))
vis_dat(op)
vis_miss(op) + 
  theme(axis.text.x = element_text(angle = 90))


#################################################################################################################################
