#pipeline to aggregate patient data ------------
# libraries required
library(dplyr)
library(lubridate)

# Method of aggregation - Option 1: Statistics --------------
# DESCRIPTION:
# Idea 1: Within t = 0 and some ending time period (e.g. t = 3 hours)
# get all readings and find some summary statistics to describe them
# mean, median, min, max, quartiles, standard deviation...
# this will then replace the normal readings.
# works for any number of readings between two time points (as long as more than 0)
# If 0: will have to just leave as completely missing
# This is why variable selection is important - 
# some variables are very empty - e.g. some obscure test
# in that case, might make sense to make it a factor - had test or not
# Could also define end time point as, e.g. time until first surgery 
# This would be useful for our created Y (fit for definitive surgery)
# If do it on a surgery-surgery basis, would then have time periods between surgeries

# FUNCTION:

get_statistics <- function(x, var, time_var, t) {
  # x: dataframe to be analysed
  # var: variable name with repeated measurements
  # time_var: name of time_var which identifiers which time reading is taken
  # t: number of hours post first reading we want to look at... 
  # Could also look at another time variable (e.g. admission?)
  # if t0 == 0 {} (to be defined properly later)
  if (class(x[[var]])=="numeric") {
    temp <- x %>% select(research_case_id, .data[[time_var]], .data[[var]]) %>%
      group_by(research_case_id) %>% 
      filter(.data[[time_var]] <= min(.data[[time_var]])+t*60*60) %>% # only look at those next hours
      # could potentially look at a different time variable (e.g. time of admission)
      # use summary function
      summarise(max = max(.data[[var]], na.rm = T), 
                min = min(.data[[var]], na.rm = T), 
                med = median(.data[[var]], na.rm = T), 
                mean = mean(.data[[var]], na.rm = T), 
                log_mean = mean(log(.data[[var]]), na.rm = T), #
                sd = sd(.data[[var]], na.rm = T), 
                lq = quantile(.data[[var]], 0.25, na.rm = T), 
                uq = quantile(.data[[var]], 0.75, na.rm = T),
                first = head(.data[[var]], 1, na.rm = T),
                last = tail(.data[[var]], 1, na.rm = T),
                n = n()) # statistics we look at. Could be added to.
    #temp[["sd"]][is.na(temp[["sd"]])] <- 0 # any NA std devs become 0 (variance of a constant = 0)
    
    # renaming the columns: 2:11 (10 stats)
    names(temp)[2:12] <- sapply(names(temp)[2:11], function(x) {paste0(var, "_",x)})
    
  }  else if (class(x[[var]])=="factor") {
    temp <- x %>% select(research_case_id, .data[[time_var]], .data[[var]]) %>%
      group_by(research_case_id) %>%
      filter(.data[[time_var]] <= min(.data[[time_var]])+t*60*60) %>% # only look at those next hours
      # could potentially look at a different time variable (e.g. time of admission)
      summarise(mode = names(which.max(table(.data[[var]]))),
                first = head(.data[[var]], 1),
                last = tail(.data[[var]], 1),
                n = n()) # statistics we look at. Could be added to.
    
    # renaming the columns: 2: mode
    names(temp)[2:5] <- sapply(names(temp)[2:5], function(x) {paste0(var, "_",x)})
  }
  temp
}
# factor and numeric? Dummy matrix. 
# filtering severe cases - pipeline -----------

mapping <- read.csv('/Users/Taru/desktop/Dataset/Mapping (all trauma).csv',
                    stringsAsFactors = T) # only severe trauma here!!

# ensure that all IDS are appropriate
case_id <- case_id <- mapping %>%
  select(research_case_id) %>%
  unique() %>%
  unlist() # 4692 research case IDs of severe trauma patients

filter_dataset <- function(data, ids = case_id){
  filtered <- data %>%
    filter(research_case_id %in% ids) %>%
    as_tibble()
  
  filtered
}

# Time period ---------

# Defining time period we are looking at
num_hrs <- 3 # in hours

# BGA_Werte -----------
# Reading in data - cleaned data with fewer NA's if appropriate
bga <- read.csv('/Users/Taru/desktop/Dataset/[SDS].[v_Export_Bga_Werte_tra_sds].csv', 
                   stringsAsFactors = TRUE) 

#bga <- filter_dataset(bga) # was not filtered initially...

bga[["Messung_dttm"]] <- as.POSIXct(bga[["Messung_dttm"]], 
                                   tz = "Europe/Zurich", 
                                   format = "%Y-%m-%d %H:%M:%OS")

# variables of importance
vars <-  names(bga)[4:ncol(bga)]   # the variables names with repeated observations

df <- get_statistics(x = bga, var = vars[1], time_var = names(bga)[3], t = num_hrs)

for (i in 2:length(vars)) {
  temp <- get_statistics(x= bga, var = vars[i], time_var = names(bga)[3], t = num_hrs)
  df <- full_join(df, temp, by = "research_case_id") # as some people may not have some things.
}

#write.csv(df, "/Users/Taru/desktop/Dataset/Stats Lab Cleaned Data/Bga_Werte_Aggregate_3hrs.csv", row.names = F)

# Repeat for other data: Vitals ------------

# Reading in data - cleaned data with fewer NA's if appropriate
vitals <- read.csv('/Users/Taru/desktop/Dataset/vitals_long.csv', 
                   stringsAsFactors = TRUE) 

vitals <- filter_dataset(vitals) 

vitals[["STARTDAT"]] <- as.POSIXct(vitals[["STARTDAT"]], 
                                       tz = "Europe/Zurich", 
                                       format = "%Y-%m-%d %H:%M:%OS")

# variables of importance
vars <-  levels(vitals[["Messung_Name"]])  # the variables names with repeated observations

# Different structure than bga as long.

vitals_sub <- vitals %>% filter(Messung_Name==vars[1])

# only looking at diastolic
df <- get_statistics(x = vitals_sub, var = "Messung_Wert", time_var = "STARTDAT", t = num_hrs)
names(df) <- gsub("Messung_Wert", vars[1], names(df))

for (i in 2:length(vars)) {
  vitals_sub <- vitals %>% filter(Messung_Name==vars[i])
  temp <- get_statistics(x = vitals_sub, var = "Messung_Wert", time_var = "STARTDAT", t = num_hrs)
  # rename due to difference in data structure
  names(temp) <- gsub("Messung_Wert", vars[i], names(temp))
  df <- full_join(df, temp, by = "research_case_id") # as some people may not have some things.
}

#write.csv(df, "/Users/Taru/desktop/Dataset/Stats Lab Cleaned Data/Vitals_Aggregate_3hrs.csv", row.names = F)

# to do --------
# change the time 0 to admissions
# gets time t hours after admission
get_time_admission <- function(hosp, t) {
  df <- hosp %>% select(research_case_id, Eintreffen_Klinik_DatumZeit,
                        EintreffenSchockraum) %>%
    mutate(Eintreffen_Klinik_DatumZeit = 
             ymd_hms(Eintreffen_Klinik_DatumZeit, tz = "Europe/Zurich"),
           EintreffenSchockraum = 
             ymd_hms(EintreffenSchockraum, tz = "Europe/Zurich"))%>% 
                mutate(end_time = ifelse(is.na(EintreffenSchockraum), 
                                         Eintreffen_Klinik_DatumZeit +  days(1) , 
                                         EintreffenSchockraum+t*60*60))
  df
  
}
