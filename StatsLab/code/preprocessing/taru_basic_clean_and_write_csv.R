#### Setting working directory ####
setwd("~/Desktop/Dataset")

#### Reading in Dataset 1 - SAPS ####

score_saps <- read.csv("[SDS].[v_Export_Score_Saps_tra_sds].csv", 
                       stringsAsFactors = T)

str(score_saps) # dimension: 4138 rows, 8 columns
sapply(sapply(score_saps, unique), length)

# name is all SAPS, taken out. 
#Research case id and research id are identifiers.
# 2 Status levels (A and I)
# 14 Senders
# 107 SAPS Value: ranging from 0-114.
# 212 Receivers
# View(score_saps)
head(score_saps)
summary(score_saps)

hist(score_saps[["SAPSValue"]])

#### Reading in Dataset 2 _ IPS ####

score_ips <- read.csv("[SDS].[v_Export_Scores_Ips_tra_sds].csv", 
                       stringsAsFactors = T)

str(score_ips) # dimension: 54174 rows, 8 columns
View(score_ips)
head(score_ips)
summary(score_ips)


#### Reading in Dataset 3 - Teilstat ####

teilstat <- read.csv("[SDS].[v_Export_Stat_Teilstat_Faelle_tra_sds].csv",
                     stringsAsFactors = T)

str(teilstat) # dimension: 20296 rows, 9 columns
View(teilstat)
head(teilstat)
summary(teilstat)

#### Reading in Dataset 4 - Investigations ####

investigation <- read.csv("[SDS].[v_Export_Untersuchungen_tra_sds].csv",
                          stringsAsFactors = T)

str(investigation) # dimension: 62902 rows, 6 columns
View(investigation)
head(investigation)
summary(investigation)


#### Reading in Dataset 5 - Vitals ####

vitals <- read.csv("[SDS].[v_Export_Vital_Werte_tra_sds].csv",
                   stringsAsFactors = T)

str(vitals) # dimension: 1 244 007 rows, 10 columns
head(vitals)
summary(vitals)
sapply(sapply(vitals, unique), length)

vitals[["STARTDAT"]] <- as.POSIXct(vitals[["STARTDAT"]], 
                     tz = "Europe/Zurich", 
                     format = "%Y-%m-%d %H:%M:%OS")

vitals[["STOPDAT"]] <- as.POSIXct(vitals[["STOPDAT"]], 
                                   tz = "Europe/Zurich", 
                                   format = "%Y-%m-%d %H:%M:%OS")
str(vitals)

#vitals <- cbind(vitals, Systolic = vitals, Diastolic)


vitals <- vitals[, c("Messung_Name", "Messung_Wert",
                     "research_case_id", "research_id",
                     "STARTDAT", "STOPDAT")]


library(tidyr)

vitals_wide <- spread(vitals, 
                      key = Messung_Name, 
                      value = Messung_Wert)



#### Gathering and writing to csv ####
# SAPS
SAPS <- read.csv("[SDS].[v_Export_Score_Saps_tra_sds].csv", 
                 stringsAsFactors = T)

SAPS <- SAPS[, c("DAT", "research_case_id",
                 "research_id", "SAPSValue")]

str(SAPS)

#write.csv(SAPS, "SAPS.csv", row.names = F)
str(read.csv("SAPS.csv"))


# investigations 
investigation <- read.csv("[SDS].[v_Export_Untersuchungen_tra_sds].csv",
                          stringsAsFactors = T)

investigation <- investigation[, c( "research_case_id",
                                   "research_id",
                                   "Observation_dttm",
                                   "Untersuchung_id",
                                   "Untersuchung_name")]
str(investigation)
#write.csv(investigation, "investigation.csv", row.names = F)

#### Vitals - clean and wide ####
vitals <- read.csv("[SDS].[v_Export_Vital_Werte_tra_sds].csv",
                   stringsAsFactors = T)

# vitals_orig <- read.csv("[SDS].[v_Export_Vital_Werte_tra_sds].csv",
#                    stringsAsFactors = T)


vitals <- vitals[, c("research_case_id", "research_id",
                     "Messung_Name", "Messung_Wert",
                     "STARTDAT", "STOPDAT")]

vitals[["STARTDAT"]] <- as.POSIXct(vitals[["STARTDAT"]], 
                                   tz = "Europe/Zurich", 
                                   format = "%Y-%m-%d %H:%M:%OS")

vitals[["STOPDAT"]] <- as.POSIXct(vitals[["STOPDAT"]], 
                                  tz = "Europe/Zurich", 
                                  format = "%Y-%m-%d %H:%M:%OS")

library(dplyr)

vitals <- distinct(vitals) 
# getting rid of any rows which are exactly identical. With initial BP.

# systolic/diastolic

levels(vitals$Messung_Name)

levels(vitals$Messung_Name) <- c(levels(vitals$Messung_Name), 
                                 "Systolic", "Diastolic")


systolic <- vitals[which(vitals[["Messung_Name"]]=="Blutdruck") , ]
systolic$Messung_Name <- "Systolic"
systolic$Messung_Wert <- sapply(strsplit(as.character(systolic$Messung_Wert),"/"),
                                function(x) {as.numeric(x[1])})

diastolic <- vitals[which(vitals[["Messung_Name"]]=="Blutdruck") , ]
diastolic$Messung_Name <- "Diastolic"
diastolic$Messung_Wert <- sapply(strsplit(as.character(diastolic$Messung_Wert),"/"),
                                function(x) {as.numeric(x[2])})

vitals <- vitals[-which(vitals[["Messung_Name"]]=="Blutdruck") , ]
vitals <- vitals[-which(vitals[["Messung_Name"]]=="Temperatur rectal") , ]

vitals[["Messung_Wert"]] <- as.numeric(as.character(vitals[["Messung_Wert"]]))

vitals <- rbind(vitals, systolic, diastolic)
head(vitals)
str(vitals)

vitals <- arrange(vitals, research_case_id, STARTDAT, Messung_Name)
library(tidyr)
# we have some rows which are identical except wrt the value.

vitals_wide <- as.data.frame(pivot_wider(vitals, 
                           names_from = Messung_Name,
                           values_from = Messung_Wert,
                           values_fn = function(x) {median(x, na.rm = T)}))


head(vitals_wide)


#write.csv(vitals_wide, "vitals.csv", row.names = F)


# Some rows are identical (including date stamps). 
#In such a case, take the median value.

# 
# try <- spread(vitals,
#                       key = Messung_Name,
#                       value = Messung_Wert)



