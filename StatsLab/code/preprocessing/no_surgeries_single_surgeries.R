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

operations <- read.csv('/Users/Taru/desktop/Dataset/operations_with_delta.csv',
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


op_hosp <- op_hosp %>%
  mutate(time_bw_er_death = as.integer(difftime(Tod_Eingetroffen_Datumzeit, Fallende_DatumZeit, units = 'mins')))

hist(op_hosp$time_bw_er_death) # max time is 48 hours - just so we know


# no surgery ####

no_operations <- case_id[! case_id %in% 
                           unique(operations_clean$research_case_id)] # case_id not in operations


no_operations <- c(as.character(no_operations), use.names= F)

mapping_no_op <- mapping[mapping$research_case_id %in% no_operations, ]

no_op_no_death <- join_data(mapping_no_op, hospital_clean) %>%
  filter(is.na(Tod_Eingetroffen_Datumzeit)) %>% select(names(mapping_no_op))

length(unique(no_op_no_death$research_case_id)) # 888 cases

# so now?

AIS_Body_regions <- no_op_no_death %>% group_by(ISS_BODY_REGION) %>% summarise(n = n()) 


ggplot(no_op_no_death, aes(x=factor(ISS_BODY_REGION))) +
  geom_bar(stat="count", fill="steelblue")+ 
  labs(x="ISS Body Region") +
  theme(axis.title=element_text(face="bold",size="14"),
        axis.text=element_text(size=14,face="bold"))
# most of these injuries were to the head

# how many of the 888 have head injuries?

# 771 out of 888 sustained head injuries!
no_op_no_death %>% group_by(research_case_id) %>% 
  filter(ISS_BODY_REGION==1) %>% select(research_case_id) %>% 
  unique() %>% dim()

# for all those non-head injuries: AIS

no_head_injuries <- no_op_no_death %>% filter(ISS_BODY_REGION!=1)

ggplot(no_head_injuries, aes(x=factor(MAX_AIS_SEVERITY))) +
  geom_bar(stat="count", fill="steelblue")+ 
  labs(x="AIS Score") +
  theme(axis.title=element_text(face="bold",size="14"),
        axis.text=element_text(size=14,face="bold"))


# no head injuries and max ais >= 3

no_head_injuries_grouped <- no_head_injuries %>%
  group_by(research_case_id) %>% filter(MAX_AIS_SEVERITY>= ais_threshold)

no_head_injuries_grouped %>% select(research_case_id) %>% 
  unique() %>% dim()

# out of these, how many did not suffer from head injury

# all those cases with head injuries
head_injury_ids <- no_op_no_death %>% group_by(research_case_id) %>%
  filter(ISS_BODY_REGION == 1) %>% select(research_case_id) %>% unique()

#

# all cases without surgery and death
no_op_no_death_id <- no_op_no_death %>% group_by(research_case_id) %>% 
  select(research_case_id) %>% unique()

# all those ids which did not obtain head injuries
no_head_injury_ids <- no_op_no_death_id$research_case_id[!(no_op_no_death_id$research_case_id %in% 
                                                             head_injury_ids$research_case_id)]

length(no_head_injury_ids)

table(mapping_no_op[ mapping_no_op$research_case_id %in% 
                       no_head_injury_ids, c("ISS_BODY_REGION", "MAX_AIS_SEVERITY")])


# single surgery ####

operations_grouped_single <- op_hosp %>% group_by(research_case_id) %>%
  summarise(n=n()) %>% filter(n==1)


# for those with single surgery:
single_surgery_ids <- operations_grouped_single$research_case_id

operations_single <- op_hosp[op_hosp$research_case_id %in% single_surgery_ids, ]


op_death_or_time <- operations_single %>%
  filter(death==1) %>% select(Delta, Totalortime, Saalbelegungzeit) # 357 died

plot(op_death_or_time$Saalbelegungzeit, pch = 19, col = "grey",
     ylab = "Saalbelegungzeit")
abline(h = 45, col = "red")
plot(op_death_or_time$Totalortime, pch = 19, col = "grey",
     ylab = "Totalortime")
abline(h = 45, col = "red")

plot(op_death_or_time$Delta, pch = 19, col = "grey",
     ylab = "Delta")
abline(h = 45, col = "red")


# surgery_v3: delta using all 3

operations_single %>%
  filter(death==1) %>%  filter(surgery_v3=="DC")


View(operations_single %>%
  filter(death==1))

operations_single %>% filter(death==0) %>% filter(surgery_v3=="DC")

# write to csv
definitive_ids <- operations_single %>% filter(death==0) %>% filter(surgery_v3=="ETC") %>% select(research_case_id)
# can be used to see who to check vitals for....

#write.csv(definitive_ids, "/Users/Taru/desktop/Dataset/single_surgery_definitive_ids.csv", row.names = F)

# looking at body regions:

# looking at ISS of all single surgery patients - can have multiple ISS
# join_data(operations_single, mapping) %>% ggplot(aes(x=factor(ISS_BODY_REGION))) +
#   geom_bar(stat="count", fill="steelblue")+ 
#   labs(x="ISS Body Region") +
#   theme(axis.title=element_text(face="bold",size="14"),
#         axis.text=element_text(size=14,face="bold"))
# 
# # ISS of all those single surgery patients who died
# join_data(operations_single, mapping) %>% filter(death==0)%>%ggplot(aes(x=factor(ISS_BODY_REGION))) +
#   geom_bar(stat="count", fill="steelblue")+ 
#   labs(x="ISS Body Region") +
#   theme(axis.title=element_text(face="bold",size="14"),
#         axis.text=element_text(size=14,face="bold"))
# 
# # ISS of all our severe trauma cases
# ggplot(mapping, aes(x=factor(ISS_BODY_REGION))) +
#   geom_bar(stat="count", fill="steelblue")+ 
#   labs(x="ISS Body Region") +
#   theme(axis.title=element_text(face="bold",size="14"),
#         axis.text=element_text(size=14,face="bold"))
# 

