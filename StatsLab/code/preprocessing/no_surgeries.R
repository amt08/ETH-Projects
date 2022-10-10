library('renv')
library('tibble')
library('dplyr')
library('ggplot2')
library('visdat')
library('naniar')
library('lubridate')


## loading data

setwd("~/Desktop/Dataset")

ais_scores_data <- read.csv('[SDS].[v_Export_Mapping_Diagnosen_ICD_to_AIS_tra_sds].csv',
                            stringsAsFactors = T)
mapping_clean <- read.csv('Mapping (all trauma).csv',
                          stringsAsFactors = T)

operations <- read.csv('[SDS].[v_Export_Operationen_Proc_tra_sds].csv',
                       stringsAsFactors = T)

hospital <- read.csv('[SDS].[v_Export_Patient_Unfall_SpitalAufenthalt_tra_sds].csv',
                     stringsAsFactors = T)


####################### Important variables ###################################

# hospital dataset
# Terapikonzept => indicator of type of surgery (e.g Early Total Care, Damage Control)
# tod_eingetroffen_datumzeit = date/time of death

# operations dataset
# Saalbelegungzeit = how long the OR was used
# Saaldttm = date/time of OR usage
# Totalortime = total time spent in the OR
# Surgeryreltime total time spent on the surgery
# Startdat
# Stopdat
# istdauer = actual duration of the surgery
# Delta = Beginnlagerungdttm - Nahtdttm

# all empty strings changed to NA

fill_na <- function(dat){
  dat <- dat %>% mutate_all(list(~na_if(., "")))
  return(dat)
}

ais_scores_data <- fill_na(ais_scores_data)
mapping_clean <- fill_na(mapping_clean)
operations <- fill_na(operations)
hospital <- fill_na(hospital)

ais_threshold <- 3

# function to filter on the right ais values for a specific dataset

ais_list <- function(data, ais_threshold){

  ais_scores_data <- as_tibble(data)

  case_id <- ais_scores_data %>% filter(MAX_AIS_SEVERITY >= ais_threshold) %>%
    select(research_case_id) %>% unique() %>% unlist()

  filtered_ais <- ais_scores_data %>% filter(research_case_id %in% case_id)

  return(filtered_ais)

}

# list of relevant research case ids

case_id <- mapping_clean %>%
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

op_hosp <- join_data(operations_clean, hospital_clean)

# create death variable
op_hosp <- op_hosp %>%
  mutate(death = ifelse(is.na(Tod_Eingetroffen_Datumzeit), 0, 1), .keep = 'all')

hospital_clean <- hospital_clean %>%
  mutate(death = ifelse(is.na(Tod_Eingetroffen_Datumzeit), 0, 1), .keep = 'all')

# transform important variables from string to datetime (not all datetime columns are transformed here)!
op_hosp <- op_hosp %>%
  mutate(Beginnlagerungdttm = ymd_hms(Beginnlagerungdttm, tz = "Europe/Zurich"),
         Nahtdttm = ymd_hms(Nahtdttm, tz = "Europe/Zurich"),
         Startdat = ymd_hms(Startdat, tz = "Europe/Zurich"),
         Stopdat = ymd_hms(Stopdat, tz = "Europe/Zurich"),
         Saaldttm = ymd_hms(Saaldttm, tz = "Europe/Zurich"),
         Erstechirmassdttm = ymd_hms(Erstechirmassdttm, tz = "Europe/Zurich"),
         Tod_Eingetroffen_Datumzeit = ymd_hms(Tod_Eingetroffen_Datumzeit, tz = "Europe/Zurich"))

# create delta variable
# op_hosp <- op_hosp %>%
#   mutate(delta = as.integer(difftime(Nahtdttm,
#                                      Beginnlagerungdttm, units = 'mins')))
# 
# # second delta variable = Schnitt - Naht
# 
# op_hosp <- op_hosp %>%
#   mutate(delta_2 = as.integer(difftime(Nahtdttm, Erstechirmassdttm, units = 'mins')))
# 


# select only few interesting duration/time variables that can act as a placeholder for delta
# or look at other potentially relevant datetime variables that can replace one of the 2:
# Beginnlagerungdttm OR Nahtdttm

# time_operations <- op_hosp %>%
#   select(delta, Startdat, Stopdat, Surgeryreltime, Saalbelegungzeit,
#          Totalortime, istdauer, Saaldttm, Beginnlagerungdttm, Nahtdttm)

# summary(time_operations)

# what is the biggest difference between delta and another 'duration' variable?

# delta_diff <- function(delta, x, variable){
#   diff <- abs(x - as.integer(delta))
# 
#   diff <- na.omit(diff)
# 
#   ggplot(data.frame(diff), aes(x = diff)) +
#     geom_histogram(fill='lightblue', color='black', bins = 50) +
#     scale_x_continuous(breaks=seq(0, 500, 25)) +
#     ggtitle(paste('Distribution of time differences between Delta and', variable)) +
#     xlab('Time difference (mins)') + ylab('Frequency') +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5))
# }
# 
# delta_diff(time_operations$delta, time_operations$istdauer, 'istdauer')
# delta_diff(time_operations$delta, time_operations$Totalortime, 'Total OR time')
# delta_diff(time_operations$delta, time_operations$Saalbelegungzeit, 'Saalbelegungzeit')
# delta_diff(time_operations$delta, time_operations$Saalbelegungzeit, 'Surgeryreltime')

################################################### Visualise data

# death rate
# op_hosp %>%
#   group_by(death) %>%
#   summarise(n = n()) %>%
#   mutate(death_rate = n / sum(n)) %>%
#   ggplot(., aes(x=death, y=death_rate)) + geom_bar(stat = 'identity')

# how many patients did not have surgery, but also didn't die
# all patients in the operations dataset should have had surgery
##### PROBLEM HERE - should not use mapping clean - FIXED #######
no_operations <- case_id[! case_id %in% 
                           unique(operations_clean$research_case_id)] # case_id not in operations
  # can we have missing data about patients who died or not?

#### A look into those who did not get surgery ####

no_operations <- c(as.character(no_operations), use.names= F)

mapping_no_op <- mapping_clean[mapping_clean$research_case_id %in% no_operations, ]
  
# this includes those who died and did not die.
# looking specifically into those who did not die

# Make Tod_Eingetroffen_Datumzeit into a date

hospital_clean <- hospital_clean %>% 
  mutate(Tod_Eingetroffen_Datumzeit = ymd_hms(Tod_Eingetroffen_Datumzeit, 
                                              tz = "Europe/Zurich"), 
         Fallende_DatumZeit = ymd_hms(Fallende_DatumZeit, 
                                      tz = "Europe/Zurich")) 

hospital_clean <- hospital_clean %>%
  mutate(time_bw_er_death = as.integer(difftime(Tod_Eingetroffen_Datumzeit, Fallende_DatumZeit, units = 'mins')))

hist(hospital_clean$time_bw_er_death) # max time is 48 hours

op_hosp <- op_hosp %>% 
  mutate(Tod_Eingetroffen_Datumzeit = ymd_hms(Tod_Eingetroffen_Datumzeit, 
                                              tz = "Europe/Zurich"), 
         Fallende_DatumZeit = ymd_hms(Fallende_DatumZeit, 
                                      tz = "Europe/Zurich")) 

op_hosp <- op_hosp %>%
  mutate(time_bw_er_death = as.integer(difftime(Tod_Eingetroffen_Datumzeit, Fallende_DatumZeit, units = 'mins')))

hist(hospital_clean$time_bw_er_death) # max time is 48 hours


# looking at those who did not die + no surgery ####

no_op_no_death <- join_data(mapping_no_op, hospital_clean) %>%
  filter(death==0) %>% select(names(mapping_no_op))

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

# 
# 
# # those who had surgery
# operations_ids <- case_id[case_id %in% 
#                              unique(operations_clean$research_case_id)]
# # case_id in operations
# # can we have missing data about patients who died or not?
# 
# 
# #### Those with multiple surgeries ####
# 
# # those who had multiple operations
# operations_grouped_multiple <- op_hosp %>% 
#   group_by(research_case_id) %>%
#   summarise(n=n()) %>% filter(n>1)
# 
# # looking into last operations.
# # order by case then start date.
# operations_hosp <- op_hosp[with(op_hosp,
#                      order(research_case_id, Startdat)), ]
# 
# operations_multiple_details <- op_hosp %>% 
#   filter(op_$research_case_id %in% 
#            operations_grouped_multiple$research_case_id)
# 
# # taking just the last operations
# operations_last_only <- operations_multiple_details %>%
#   group_by(research_case_id) %>% 
#   summarise(across(everything(), last))
# 
# summary(operations_last_only)
# 
# 
# 


#### Single surgeries ####

op_hosp <- op_hosp %>% 
  mutate(Tod_Eingetroffen_Datumzeit = ymd_hms(Tod_Eingetroffen_Datumzeit, 
                                              tz = "Europe/Zurich"), 
         Fallende_DatumZeit = ymd_hms(Fallende_DatumZeit, 
                                      tz = "Europe/Zurich")) 

op_hosp <- op_hosp %>%
  mutate(time_bw_er_death = as.integer(difftime(Tod_Eingetroffen_Datumzeit, 
                                                Fallende_DatumZeit, units = 'mins')))


operations_grouped_single <- op_hosp %>% group_by(research_case_id) %>%
  summarise(n=n()) %>% filter(n==1)


# for those with single surgery:
single_surgery_ids <- operations_grouped_single$research_case_id

operations_single <- op_hosp[op_hosp$research_case_id %in% single_surgery_ids, ]

#summary(operations_single) # still some NA

# who died

operations_single %>%
  filter(death==1) %>% dim() # 357

# hist(operations_single$time_bw_er_death, breaks = 10) # within 48 hours, death occured

# Delta ####

# Original:

#operations_single <- operations_single %>% 
#  mutate(Delta = as.integer(difftime(Stopdat, Beginnlagerungdttm, units = 'mins'))) # Stopdat == Nahtdttm






# no_death_single_surgery_dc <- operations_single %>%
#   filter(is.na(Tod_Eingetroffen_Datumzeit)) %>% filter(delta<60)
# 
# 
# no_death_single_surgery_def <- operations_single %>%
#   filter(is.na(Tod_Eingetroffen_Datumzeit)) %>% filter(delta>=60)





# # Make Tod_Eingetroffen_Datumzeit into a date
# 
# hospital_clean <- hospital_clean %>% 
#   mutate(Tod_Eingetroffen_Datumzeit = ymd_hms(Tod_Eingetroffen_Datumzeit, 
#                                       tz = "Europe/Zurich"), 
#          Fallende_DatumZeit = ymd_hms(Fallende_DatumZeit, 
#                                       tz = "Europe/Zurich")) 
# 
# hospital_clean <- hospital_clean %>%
#   mutate(time_bw_er_death = as.integer(difftime(Tod_Eingetroffen_Datumzeit, Fallende_DatumZeit, units = 'mins')))
# 
# 
# no_death_surgery <- mapping_clean %>%
#   left_join(hospital_clean, by = c('research_case_id', 'research_id')) %>%
#   filter(is.na(Tod_Eingetroffen_Datumzeit)) %>%
#   left_join(operations_clean, by = c('research_case_id', 'research_id')) %>%
#   filter(is.na(Startdat)) %>%
#   select(research_case_id, research_id) %>%
#   unique()
# 
# # we have 888 of 4692 cases who didn't die and didn't have an operation
# no_death_surgery %>% dim()
# 
# mapping_clean %>% select(research_case_id, research_id) %>%
#   unique() %>% dim()
# 
# 
# # converting more to dates
# 
# op_hosp_dates <- grep("dttm", names(op_hosp))
# op_hosp_dates <- c(op_hosp_dates, grep("Datum", names(op_hosp)))
# 
# for (i in op_hosp_dates) {
#   name <- names(op_hosp)[i]
#   op_hosp[[name]] <- as.POSIXct(op_hosp[[name]],
#                                 tz = "Europe/Zurich",
#                                 format = "%Y-%m-%d %H:%M:%OS")
# }
# 
# 
# 
# 
# 
# # data summary in merging operations with hospital
# vis_dat(op_hosp)
# vis_dat(time_operations)
# 
# # missingness plot
# vis_miss(op_hosp)
# 
# gg_miss_var(op_hosp)
# 
# ######################################################
# 
# operations_clean %>%
#   select(research_id, research_case_id) %>%
#   unique() %>%
#   dim()
# 
# 
# unique_cases <- function(x) {
#   length(unique(x[["research_case_id"]]))
# }
# 
# 
# # maybe some spaghetti plots?
# vitals <- read.csv("vitals.csv", stringsAsFactors = T)
# 
# 
# vitals[["STARTDAT"]] <- as.POSIXct(vitals[["STARTDAT"]], 
#                                    tz = "Europe/Zurich", 
#                                    format = "%Y-%m-%d %H:%M:%OS")
# 
# vitals[["STOPDAT"]] <- as.POSIXct(vitals[["STOPDAT"]], 
#                                   tz = "Europe/Zurich", 
#                                   format = "%Y-%m-%d %H:%M:%OS")
# 
# vitals_cleaned <- filter_dataset(vitals, case_id)
# 
# 
# # subset the data so we only look at a few individuals for spaghetti plot
# # in the same time frame
# vitals_sub <- vitals_cleaned[vitals_cleaned$STARTDAT>as.Date("2021-01-14"),
#                              c(1, 3, 6)]
# 
# vitals_sub <- vitals_sub[complete.cases(vitals_sub), ]
# 
# 
# # spaghetti plot
# ggplot(data = vitals_sub,
#        aes(x = STARTDAT, y = Temperatur, col = research_case_id)) +
#   geom_line() + geom_point() + 
#   ylab("Temperature") + xlab("Time of Measurement") + 
#   theme_bw() +
#   theme(legend.position = "none") 
# # 
# 
# # make this spaghetti plot into a function
# 
