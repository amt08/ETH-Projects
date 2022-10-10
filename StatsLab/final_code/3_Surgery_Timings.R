# LONG FORMAT --------
# Operations timing of surgeries creation -------
library(dplyr)
library(readr)
library(lubridate)

# set working directory to data folder - Contains all: A-S cleaned data files, as per 09.05.2021
setwd("~/Desktop/Data Folder - StatsLab - May 21")
my_repo <- "~/Documents/ETH/Stats Lab/"
source(paste0(my_repo, "polytrauma/code/preprocessing/helper_functions.R"))

# Defining time between surgeries ----------
time_points_bw_surgeries <- function(operations, starting = "Startdat", ending = "Stopdat") {
  #browser()

  all_surgeries <- operations %>% select(-SurgeryNr) %>% group_by(research_case_id)%>% 
    arrange(.data[[starting]])  %>% mutate(SurgeryNr = row_number()) %>%
    group_by(research_case_id, SurgeryNr) %>% 
    mutate(start = .data[[starting]],
           end = .data[[ending]]) %>%
    select(research_case_id, SurgeryNr, start, end)
  
  all_surgeries
}

# creation -----------
operations <- read.csv("H_Operationen.csv")

length(unique(operations$research_case_id)) # only 1131 patients? How?

operations <- operations %>% mutate(Startdat = ymd_hms(Startdat, tz = "Europe/Zurich"),
                                    Stopdat = ymd_hms(Stopdat, tz = "Europe/Zurich"))


surgery_times <- time_points_bw_surgeries(operations) 
# writing to file --------

#write.csv(surgery_times, "H_Operationen_Surgery_Times_long.csv", row.names = F)
save(surgery_times, file = "H_Operationen_Surgery_Times_long.RData")

