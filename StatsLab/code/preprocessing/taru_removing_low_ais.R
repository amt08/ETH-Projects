#### Patients with highest AIS less than 3 ####
setwd("~/Desktop/Dataset")

ais <- read.csv("[SDS].[v_Export_Mapping_Diagnosen_ICD_to_AIS_tra_sds].csv",
                stringsAsFactors = T)

vitals <- read.csv("vitals.csv",stringsAsFactors = T )
SAPS <- read.csv("SAPS.csv", stringsAsFactors = T)
investigation <- read.csv("investigation.csv", stringsAsFactors = T)

op <- read.csv("[SDS].[v_Export_Operationen_Proc_tra_sds].csv", stringsAsFactors = T)



ais <- ais[, c("Dat", "MAX_AIS_SEVERITY", "research_case_id", "research_id")]

orig_cases <- numeric(3)

length(unique(ais$research_case_id)) # 17214
orig_cases[1] <- length(unique(vitals$research_case_id)) # some cases may not have ais scores. 19870
orig_cases[2] <-length(unique(SAPS$research_case_id)) #3143
orig_cases[3] <-length(unique(investigation$research_case_id)) #14233

length_unique_2vec <- function(x, y) {
  ind <- which(x %in% y)
  vals <- x[ind]
  length(unique(vals))
}

intersection <- numeric(3)

intersection[1] <- length_unique_2vec(vitals$research_case_id, ais$research_case_id) # 12621

intersection[2] <- length_unique_2vec(SAPS$research_case_id, ais$research_case_id) # 2920

intersection[3] <- length_unique_2vec(investigation$research_case_id, ais$research_case_id) # 16936

# 2934 vitals cases not in ais, 223 saps and 1612 investigations.

# for those present in ais, let's eliminate those with max ais <= 2

include <- unique(ais$research_case_id[ais$MAX_AIS_SEVERITY>2])

exclude <- unique(ais$research_case_id[!ais$research_case_id %in% include])

# 12522 to be excluded

vitals_filtered <- vitals[!vitals$research_case_id %in% exclude ,]

SAPS_filtered <- SAPS[!SAPS$research_case_id %in% exclude, ]

investigation_filtered <- investigation[!investigation$research_case_id %in% exclude, ]

#write.csv(vitals_filtered, "vitals_filtered.csv", row.names = F)
#write.csv(investigation_filtered, "investigation_filtered.csv", row.names = F)
#write.csv(SAPS_filtered, "SAPS_filtered.csv", row.names = F)

new_cases <- numeric(3)
new_cases[1] <- length(unique(vitals_filtered$research_case_id))
new_cases[2] <-length(unique(SAPS_filtered$research_case_id))
new_cases[3] <-length(unique(investigation_filtered$research_case_id))

# useful dplyr code

library(dplyr)

op.sum <- op %>% group_by(research_case_id) %>%
  summarise(nop = n())


ais.group <- ais %>% group_by(research_case_id) %>%
  summarise(n = n(), max = max(.data[["MAX_AIS_SEVERITY"]])) %>%
  filter(n>1) %>% filter(max>2)

ais.op <- merge(ais.group, op.sum, by = "research_case_id")

nrow(ais.op  %>%
  filter(n>1) %>% filter(max>2) %>% filter(nop>1))

  # filter accordingly


## What if we only include poly + max AIS > 2? --------

# going to exclude anyone without AIS





