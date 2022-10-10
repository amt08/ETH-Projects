library(dplyr)

# some EDA
setwd("Desktop/Dataset")


vitals <- read.csv("vitals.csv", stringsAsFactors = T)

ais <- read.csv("[SDS].[v_Export_Mapping_Diagnosen_ICD_to_AIS_tra_sds].csv",
                stringsAsFactors = T)

ais <- ais[, c("Dat", "MAX_AIS_SEVERITY", "research_case_id", "research_id")]

include <- unique(ais$research_case_id[ais$MAX_AIS_SEVERITY>2])

ais.group <- ais %>% group_by(research_case_id) %>%
  summarise(n = n(), max = max(.data[["MAX_AIS_SEVERITY"]])) %>%
  filter(n>1) %>% filter(max>2)


vitals.group <- vitals %>% group_by(research_case_id) %>%
  summarise(n = n())

poi <- vitals.group$research_case_id[which.max(vitals.group$n)]

poi <- vitals[which(vitals$research_case_id==poi) , ]


summary(poi)

poi$STARTDAT <- as.POSIXct(poi$STARTDAT , tz = "Europe/Zurich",
                           format = "%Y-%m-%d %H:%M:%OS")

poi <- poi[, c(1, 2, 3, 5)]
summary(poi)
poi <- poi[complete.cases(poi), ]

plot( poi$Puls~poi$STARTDAT, type="l")

