# Creating Y ---------------------
setwd("~/Desktop/Dataset")

ais <- read.csv("[SDS].[v_Export_Mapping_Diagnosen_ICD_to_AIS_tra_sds].csv",
                stringsAsFactors = T)

max_ais <- 3 # Maximum AIS must be this value or greater


# the only ID's of interest to us.
# AIS greater or equal to max_ais and have an existing AIS.
case_id <- unique(ais$research_case_id[ais$MAX_AIS_SEVERITY>=max_ais])

# next: Let's define those who got surgery and those who didn't

operation <- read.csv("[SDS].[v_Export_Operationen_Proc_tra_sds].csv",
                      stringsAsFactors = T)
# delta: Beginnlagerungdttm - Nahtdttm

### vitals[["STARTDAT"]] <- as.POSIXct(vitals[["STARTDAT"]], tz = "Europe/Zurich", format = "%Y-%m-%d %H:%M:%OS")

variables <- c("research_case_id", "research_id", "Beginnlagerungdttm",
               "Nahtdttm")

operation <- operation[ , variables]

operation[["Beginnlagerungdttm"]] <- as.POSIXct(operation[["Beginnlagerungdttm"]],
                                                tz = "Europe/Zurich", format = "%Y-%m-%d %H:%M:%OS")

operation[["Nahtdttm"]] <- as.POSIXct(operation[["Nahtdttm"]],
                                                tz = "Europe/Zurich", format = "%Y-%m-%d %H:%M:%OS")

operation <- as.data.frame(cbind(operation, delta = difftime(operation$Nahtdttm-operation$Beginnlagerungdttm)))

