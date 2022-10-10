# EDA for multiple surgeries. How many AIS’s do they have, how many damage control, how many definitive.
# Explore potential Deltas? And start defining and converting some


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

op_2 <- read.csv('data/raw_data/[SDS_DGU].[v_Export_Operation_Termin_proc]_extra.csv', stringsAsFactors = T)

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

################ No death + No operation

# no_operations <- mapping %>%
#   left_join(op_hosp, by = c('research_case_id', 'research_id')) %>%
#   filter(is.na(Startdat))
# 
# no_death_surgery <- mapping %>%
#   left_join(hosp, by = c('research_case_id', 'research_id')) %>%
#   filter(is.na(Tod_Eingetroffen_Datumzeit)) %>%
#   left_join(op, by = c('research_case_id', 'research_id')) %>%
#   filter(is.na(Startdat)) %>%
#   select(research_case_id, research_id) %>%
#   unique()
# 
# # we have 888 of 4692 cases who didn't die and didn't have an operation
# no_death_surgery %>% dim()

###########################################################

### time-dependent variables

# only look at the operations dataset for now and keep the death variable
operate <- op_hosp %>% select(c(colnames(op), 'death', 'Eintreffen_Klinik_DatumZeit', 'EintreffenSchockraum'))

operate %>% filter(is.na(`ï..abgabedttm`)) # 735 missing values
# abgabedttm (Arrival time?) is recorded later than Startdat. so what is that?

operate %>% filter(is.na(EintreffenSchockraum)) # this has many missing values and so can't really be used neither the other one, since
# it doesn't record the time

operate <- operate %>%
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
         EintreffenSchockraum = ymd_hms(EintreffenSchockraum, tz = "Europe/Zurich"))

# create delta with what we have available

operate <- operate %>% mutate(Delta = as.integer(difftime(Nahtdttm, Beginnlagerungdttm, units = 'mins')))

vis_dat(operate)

operate %>%
  filter(is.na(Delta)) %>%
  dim() # 4586 missing values of 8322 ~ 55% missing values

operate %>%
  filter(is.na(Beginnlagerungdttm)) %>%
  dim() # Beginnlagerungdttm = 4538 missing valuess

operate %>%
  filter(is.na(Nahtdttm)) %>%
  dim() # 2319 Nahtdttm missing values

vis_miss(operate, sort_miss = TRUE)

# if we replace with TotalORTime, how many missing values do we still have left?

operate <- operate %>%
  mutate(totalortime_repl = coalesce(Delta, Totalortime)) # 8.5% missing values

# if we replace with SurgeryReltime, how many missing values do we still have left?

operate <- operate %>%
  mutate(surgeryrel_repl = coalesce(Delta, Surgeryreltime))

# if we replace with Saalbelegungzeit, how many missing values do we still have left? 

operate <- operate %>%
  mutate(Saalbelegungzeit_repl = coalesce(Delta, Saalbelegungzeit))

hist(operate$Saalbelegungzeit_repl, breaks = 150)

# if we replace with istdauer, how many missing values do we still have left? 27% of missing values

operate <- operate %>%
  mutate(istdauer_repl = coalesce(Delta, istdauer))

hist(operate$istdauer, breaks = 150)

operate %>%
  filter(istdauer == 0) # 6 operations with 0 delta

operate %>%
  filter(istdauer < 10)

# is Stopdat - Startdat the same as istdauer?

operate <- operate %>%
  mutate(diff = as.integer(difftime(Stopdat, Startdat, units = 'mins')))

hist(operate$diff, breaks = 150) # seems like we have a few values of 0
operate %>%
  filter(diff < 10)

operate <- operate %>%
  mutate(diff_istdauer = abs(istdauer - diff))

max(operate$diff_istdauer, na.rm = TRUE)

plot(operate$diff_istdauer) # seems like we have a few values different from 0

istdauer <- operate %>%
  filter(diff_istdauer != 0) # 25 values different from 0


# name of variabiles
# abgabedatum = Submission date / arrival date
# ausleitendedttm = Drainage date?
# ausleitungzeit = duration of drainage
# Aussaaldttm = exit date / release date
# Einleitbeginndttm, Einleitendedttm, Einleitungzeit = Initiation: start date, end date, duration
# Saalbelegungzeit = room occupancy time

head(operate)

missing <- operate %>%
  filter(is.na(totalortime_repl))

operate <- operate %>%
  mutate(Final = coalesce(Delta, Totalortime, Saalbelegungzeit))



vis_dat(operate)
library('naniar')
gg_miss_var(operate, show_pct = TRUE)

prep_data <- operate %>%
  select(-c(Delta, totalortime_repl, Saalbelegungzeit_repl, death,
            surgeryrel_repl, diff, Final, research_case_id, research_id, diff_istdauer, istdauer_repl))

vis_miss(prep_data, sort_miss = TRUE) + 
  theme(axis.text.x = element_text(angle = 90))
gg_miss_var(prep_data, show_pct = TRUE)


################################################################################################################################

#####creating the definitive surgery vs. damage control - version 1, using Delta and Stopdat-Startdat

operate <- operate %>% mutate(Start_delta = as.integer(difftime(Stopdat, Startdat, units = 'mins')))

operate <- operate %>% mutate(surgery_v1 = ifelse(!is.na(Delta), ifelse(Delta >= 60, 'ETC', 'DC'),
                                   ifelse(!is.na(Start_delta), ifelse(Start_delta >= 50, 'ETC', 'DC'), NA)))

#####creating the definitive surgery vs. damage control - version 2, using Delta and TotalORtime

operate <- operate %>% mutate(surgery_v2 = ifelse(!is.na(Delta), ifelse(Delta >= 60, 'ETC', 'DC'),
                                                        ifelse(!is.na(Totalortime), ifelse(Totalortime >= 45, 'ETC', 'DC'), NA)))


#####creating the definitive surgery vs. damage control - version 3, using Delta, TotalORtime and Saalbelegungzeit

operate <- operate %>%
  mutate(surgery_v3 = ifelse(!is.na(Delta), ifelse(Delta >= 60, 'ETC', 'DC'),
                           ifelse(!is.na(Totalortime), ifelse(Totalortime >= 45, 'ETC', 'DC'),
                                  ifelse(!is.na(Saalbelegungzeit), ifelse(Saalbelegungzeit >= 45, 'ETC', 'DC'), NA))))
vis_dat(operate)


#################################################################################################################################

#### conclusions:
## is we look at the discrepancy between Stopdat and Nahtddtm they are merely
# the same in case where, of course, we can compare (ie. Nahtdttm has no missing values). only 18 operations are different between the 2 times.

### let's see how much of a gap is between Startdat and Beginnlagerungdttm

test <- operate %>% mutate(diff_start = as.integer(difftime(Startdat, Beginnlagerungdttm, units = 'mins')))

hist(test$diff_start, breaks = 200) # on average seems like there is a 30 mins difference

############################## if we eliminate diagnostic from dataset, how many missing values do we still have left?

filt_op <- op_2 %>% 
  mutate(Termtxt = tolower(Termtxt)) %>%
  filter(!grepl("diagnostik|diagnose|diagnosik|diagnsotik|diagnostk|sr-diagn|diagnostok", Termtxt))

missing_surg <- filt_op %>% filter(is.na(Surgeryreltime))

tot_or <- mis %>% filter(is.na(Totalortime))

missing_tot <- filt_op %>% filter(is.na(Totalortime))
