# LONG FORMAT
# Operations timing of surgeries creation
# Creates a data frame consisting of all case IDs, surgery numbers and their start and stop timings. To be used for aggregation.

library(dplyr)
library(readr)
library(lubridate)

# set working directory to data folder
setwd("~/Desktop/Data Folder - StatsLab - May 21")
my_repo <- "~/Documents/ETH/Stats Lab/"
source(paste0(my_repo, "polytrauma/code/preprocessing/helper_functions.R"))

# Defining time between surgeries
time_points_bw_surgeries <- function(operations, starting = "Startdat", ending = "Stopdat") {
  all_surgeries <- operations %>% select(-SurgeryNr) %>% group_by(research_case_id)%>% 
    arrange(.data[[starting]])  %>% mutate(SurgeryNr = row_number()) %>%
    group_by(research_case_id, SurgeryNr) %>% 
    mutate(start = .data[[starting]],
           end = .data[[ending]]) %>%
    select(research_case_id, SurgeryNr, start, end)
  
  all_surgeries
}

# creation
operations <- read.csv("H_Operationen.csv")

operations <- operations %>% mutate(Startdat = ymd_hms(Startdat, tz = "Europe/Zurich"),
                                    Stopdat = ymd_hms(Stopdat, tz = "Europe/Zurich"))

surgery_times <- time_points_bw_surgeries(operations) 

# writing to file
save(surgery_times, file = "H_Operationen_Surgery_Times_long.RData")
