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
  op_times <- data.frame()
  
  first_surgeries <- operations %>% 
    group_by(research_case_id) %>% 
    arrange(.data[[starting]]) %>% 
    mutate(start_1 = head(.data[[starting]], 1),
           end_1 = head(.data[[ending]], 1)) %>%
    select(research_case_id, start_1, end_1)
  
  op_times <- first_surgeries
  
  op_n <- operations %>% 
    group_by(research_case_id) %>% 
    arrange(.data[[starting]]) %>% 
    mutate(n = n()) 
  
  temp <- op_n %>% filter(n >= 2) %>% # those with 2 surgeries
    mutate(start_2 = head(.data[[starting]], 2)[2],
           end_2 = head(.data[[ending]], 2)[2]) %>%
    select(research_case_id, start_2, end_2) %>%
    distinct()
  
  op_times <- merge(op_times, temp, by = "research_case_id", all.x = T)
  
  for (i in 3:max(op_n$n)) {
    print(i)
    temp <- op_n %>% filter(n >= i) %>% 
      select(research_case_id, .data[[starting]], .data[[ending]], n) %>%
      mutate("start_{i}" := head(.data[[starting]], i)[i],
             "end_{i}" := head(.data[[ending]], i)[i] ) %>%
      select(-.data[[starting]], -.data[[ending]], -n) %>%
      distinct()
    
    op_times <- merge(op_times, temp, by = "research_case_id", all.x = T)
  }
  op_times %>% distinct()
}

# creation -----------
operations <- read.csv("H_Operationen.csv")
operations <- filter_dataset(operations) # why is this only 2380 records now?

length(unique(operations$research_case_id)) # only 1131 patients? How?

operations <- operations %>% mutate(Startdat = ymd_hms(Startdat, tz = "Europe/Zurich"),
                     Stopdat = ymd_hms(Stopdat, tz = "Europe/Zurich"))


surgery_times <- time_points_bw_surgeries(operations) 
# writing to file --------

#write.csv(surgery_times, "H_Operationen_Surgery_Times_7164.csv", row.names = F)
#save(surgery_times, file = "H_Operationen_Surgery_Times_7164.RData")

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





