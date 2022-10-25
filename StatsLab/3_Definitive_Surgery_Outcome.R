
# This script identifies the outcome of definitive surgeries.
# It compares pre- and post-outcomes of selected variables and then
# builds decisions based on certain rules

##### Functions #####

# [Not Used] Surgery time and date of given research case id
# If multiple surgeries, it will also return the function of the subsequent surgery
# Note that treatment time is the time indicated in Sascha's DCO_Defsurg.xlsx file
find.surgery.time <- function(id, stopdat = NA, operations.data){
  
  stopdat <- as.POSIXlt(stopdat, format = "%Y-%m-%d %H:%M:%S")
  
  # only keep
  operations.data <- operations.data %>%
    # relevant cases
    filter(research_case_id == id) %>%
    # surgeries with stopdat == stopdat or later
    # if multiple surgeries but only 1 obs left, it was the last surgery
    filter(DtTm >= stopdat)
  
  # if no surgery
  if (nrow(operations.data) == 0){
    return(NA)
  }
  
  # if multiple surgeries
  if (nrow(operations.data) > 1){
    # No stopdat given
    if (is.na(stopdat)){return(NA)}
    
    # if multiple surgeries find the one closest to stopdat
    operations.data <- operations.data %>%
      mutate(time.delta = DtTm - stopdat)
      arrange(time.delta)
    
    # also return subsequent surgery
    relevant_dates <- operations.data$DtTm[1:2]
    return(relevant_dates)
  }
  
  # if single surgery
  relevant_date <- operations.data$DtTm[1]
  return(relevant_date)
}

# [Not Used] find h-hours time window before/after t hours of surgery
find.time.window <- function(t, h, id){
  # get surgery time
  surgery.time <- find.surgery.time(id)
  # t hours post/pre surgery time
  delta.surgery.time <- survery.time + 60*60*t # additions in secs
  # h hours window
  delta.surgery.window <- delta.surgery.time + c(-1, 1)*60*60*h/2
  
  return(delta.surgery.window)
}

# [Not Used] h hours after surgery vital for descriptive
# this function is used for summary statistics and shows where and why data is missing
# note that datatime variable of data needs to be POSIXlt and in a variable named DtTm
find.vitals.descriptive <- function(h, id, data, vital){
  
  # get surgery time
  find.surgery <- find.surgery.time(id)
  
  # exit if no surgery time
  if (is.na(find.surgery)){
    return("No Surgery Time")
  }
  
  # get time h hours after surgery
  after.surgery <- find.surgery+60*60*h # addition is secs
  
  # filter data by relevant patient
  data <- data %>%
    filter(research_case_id == id) # relevant patient
  
  # exit if no obs left
  if (nrow(data) == 0){
    return("Patient not in data")
  }
  
  # filter data by missing values
  data <- data %>%
    filter(vital != "" | !is.na(vital)) # non-missing vital
  
  # exit if no obs left
  if (nrow(data) == 0){
    return("Vital only with missing")
  }
  
  # filter data by missing values
  data <- data %>%
    filter(DtTm >= after.surgery) # relevant patient
  
  # exit if no obs left
  if (nrow(data) == 0){
    return("No obs. in filtered time window")
  }
  
  data <- data[vital]
  
  # return the number of obs
  return(nrow(data))
}

# [Not Used] Error counter for descriptive
error.counter <- function(error.vector){
  error.frame <- matrix(nrow = 5, ncol = 1)
  rownames(error.frame) <- c("Average obs. h hours after surgery",
                             "Patient not in data",
                             "No obs. in filtered time window",
                             "Vital only with missing",
                             "No Surgery Time")
  
  error.frame[1,] <- mean(as.numeric(error.vector), na.rm = TRUE)
  error.frame[2,] <- sum(error.vector == "Patient not in data")
  error.frame[3,] <- sum(error.vector == "No obs. in filtered time window")
  error.frame[4,] <- sum(error.vector == "Vital only with missing")
  error.frame[5,] <- sum(error.vector == "No Surgery Time")
  return(error.frame)
}

# Find subsequent surgery
# If multiple surgeries, return the date time of the subsequent surgery
temp <- numeric(nrow(patients))
for (i in 1:nrow(patients)){
  temp[i] <- find.second.surgery(patients$research_case_id[i], patients$DtTmStop[i], operations)
}

find.second.surgery <- function(id, stopdat, operations.data){
  stopdat <- as.POSIXlt(stopdat, format = "%Y-%m-%d %H:%M:%S")
  
  # only keep
  operations.data <- operations.data %>%
    # relevant cases
    filter(research_case_id == id) %>%
    # surgeries with stopdat == stopdat or later
    # if multiple surgeries but only 1 obs left, it was the last surgery
    filter(DtTmStop >= stopdat) %>%
    arrange(DtTmStop)

  # if multiple surgeries
  if (nrow(operations.data) > 1){
    # if multiple surgeries find the one closest to stopdat
    relevant_dates <- operations.data$DtTmStop[2]
    # return subsequent surgery
    return(relevant_dates)
  }
  # if single surgery or no surgery
  return(NA)
}

# Development of post-surgery vital value
# this function returns whether a provided vital value developed positively (= 1) or negatively (= 0)
# note that datatime variable of data needs to be POSIXlt and in a variable named DtTm
temp <- numeric(nrow(patients))
for(i in 1:nrow(patients)){
  temp[i] <- vital.outcome(patients$research_case_id[i], patients$DtTmStop[i], blut, "QUICK", operations)
}

vital.outcome <- function(id, surgery.time, data, vital, operations.data){
  
  data.orig <- data
  # filter data by
  data <- data %>%
    filter(research_case_id == id) # relevant patient
    
  # exit if no obs left
  if (nrow(data) == 0){
    return("No Patient")
  }
  
  # filter data by
  data <- data %>%
    filter(get(vital) != "" | !is.na(get(vital))) # no missing values
    
  # exit if no obs left
  if (nrow(data) == 0){
    return("Vitals Missing")
  }
  
  # get subsequent surgery (if available)
  second.surgery <- find.second.surgery(id, surgery.time, operations.data)
  
  # If single surgery
  if (is.na(second.surgery)){
    # filter data by
    data <- data %>%
      filter(DtTm >= surgery.time) # only vitals after surgery
  }
  # If multiple surgeries
  else{
    # filter data by
    data <- data %>%
      filter(DtTm >= surgery.time & DtTm < second.surgery) # only vitals in between surgeries
  }

  # exit if no obs left
  if (nrow(data) == 0){
    return("Vitals not in Time Window")
  }
  
  # code as 1 if only one obs after surgery ()
  if (nrow(data) == 1){
    return(1)
  }
  
  # trend between 1st and 2nd obs
  data <- data %>% 
    arrange(DtTm) %>%
    top_n(-2, DtTm)
  
  # check if values are numeric
  is.number <- !is.na(suppressWarnings(as.numeric(data[,vital])))
  # if 1st and 2nd obs are numeric
  if (all(is.number)) {
    trend <- as.numeric(data[2,vital]) - as.numeric(data[1,vital])
    }
  
  # non-numeric but category exists
  else{
    # if category variable exists and is not already data
    if (paste0(vital, "_Categ") %in% colnames(data) & !grepl("_Categ", vital)){
      browser()
      return("Vitals is Category")
    
    }
    else{
      # trend not possible to calculate
      return("Trend not possible")}
  }
  
  # variables for which a increase is positive
  increase <- c("CKDEPI", "NEUA", "FBG", "QUICK", "APTT", "T", 
                "Temperatur", "GCS", "po2", "FIO2", "MAP", "tHb")
  
  # variables for which a decrease is positive
  decrease <- c("Lac", "BAS", "BASA", "CRP", "IMGR", "IMGRA", "LC",
                "LYM", "NEU", "PCT", "PCTB", "TC", "GGT", "KHINR",
                "PTZEIT", "TZI", "HUFH", "CK", "LDH", "MYO", "Puls")
  
  if (sign(trend) == 1 & vital %in% increase){return(abs(trend))}
  if (sign(trend) == -1 & vital %in% decrease){return(abs(trend))}
  else{return(-abs(trend))}
  
}

# Clinical Course
# returns positive / negative outcome for all clinical courses variables from https://polytraumacourse.com/clinical-course-12-h
# each row is a surgery and each column a variable from the clinical course
ClinicalCourse.outcome <- function(id, surgery.time, Clinical.Cours.vars, operations.data) {

  n.vars <- nrow(Clinical.Cours.vars)
  vital.outcomes <- numeric(n.vars)
  names(vital.outcomes) <- Clinical.Cours.vars$Variable.Name
  for (i in 1:n.vars){
    vital <- Clinical.Cours.vars$Variable.Name[i]
    vitalDataset <- Clinical.Cours.vars$Data[i]
    vital.outcomes[i] <- vital.outcome(id, surgery.time, get(vitalDataset), vital, operations.data) 
  }
  return(vital.outcomes)
}

# Weighted Mean
# Apply a weighted mean on the clinical course variables
weight.ClinicalCourse.outcome <- function(ClinicalCourse, weight.vector){
  
  # if list is supplied make it to 1 row df
  if ("character" %in% class(ClinicalCourse)){
    ClinicalCourse <- data.frame(lapply(ClinicalCourse, function(x) t(data.frame(x))))
  }
  
  # keep only numeric
  suppressWarnings(
    ClinicalCourse <- as_tibble(ClinicalCourse) %>%
      mutate_all(as.numeric))
  
  # calculate the weighted outcome
  weight.outcome <- numeric()
  for (i in 1:nrow(ClinicalCourse)){
    weight <- weight.vector[!is.na(ClinicalCourse[i,])]
    
    # if no variable with non-NA value
    if (length(weight)==0){
      weight.outcome[i] <- NA
      next
    }
    weight <- weight/sum(weight)
    weight.outcome[i] <- weighted.mean(ClinicalCourse[i,which(!is.na(ClinicalCourse[i,]))], weight)
  }
  return(weight.outcome)
}

# packages and set directory
library(tidyverse)
                                        
# Load Relevant Patients and all Operations
patients <- read.csv("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/T_Outcome_Y_Coded.csv") %>%
  mutate(DtTmStop = as.POSIXlt(Stopdat, format = "%Y-%m-%d %H:%M:%S") ) %>%
  mutate(DtTmStart = as.POSIXlt(Startdat, format = "%Y-%m-%d %H:%M:%S") )
  
operations <- read.csv("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/H_Operationen.csv") %>%
  mutate(DtTmStop = as.POSIXlt(Stopdat, format = "%Y-%m-%d %H:%M:%S") ) %>%
  mutate(DtTmStart = as.POSIXlt(Startdat, format = "%Y-%m-%d %H:%M:%S") )
  
# Load vital data
clinical.course.variables <- read.csv("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/ClinicalCourseVariables.csv")

blut <- read.csv("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/B_Blut_Werte_Categorized.csv") %>%
  mutate(DtTm = as.POSIXlt(ObservationDtTm, format = "%Y-%m-%d %H:%M:%S") ) %>%
  mutate(APTT_Categ = ifelse(APTT_Categ == "[0,16]", "(0,16]", APTT_Categ)) %>%
  mutate(APTT_Categ = factor(APTT_Categ)) %>%
  mutate(FBG_Categ = ifelse(FBG_Categ == "[0,16]", "(0,16]", FBG_Categ)) %>%
  mutate(FBG_Categ = factor(FBG_Categ)) %>%
  mutate(KHINR_Categ = ifelse(KHINR_Categ == "[0,16]", "(0,16]", KHINR_Categ)) %>%
  mutate(KHINR_Categ = factor(KHINR_Categ)) %>%
  mutate(MYO_Categ = ifelse(MYO_Categ == "[0,16]", "(0,16]", MYO_Categ)) %>%
  mutate(MYO_Categ = factor(MYO_Categ)) %>%
  mutate(QUICK_Categ = ifelse(QUICK_Categ == "[0,16]", "(0,16]", QUICK_Categ)) %>%
  mutate(QUICK_Categ = factor(QUICK_Categ)) %>%
  mutate(TZI_Categ = ifelse(TZI_Categ == "[0,16]", "(0,16]", TZI_Categ)) %>%
  mutate(TZI_Categ = factor(TZI_Categ))

# Adjust the variable CK before Scaling
# Filter out Text elements and their position
text <- blut$CK[is.na(as.numeric(blut$CK)) != is.na(blut$CK)]
position <- is.na(as.numeric(blut$CK)) != is.na(blut$CK)
blut$CK <- as.numeric(blut$CK)

# Scale relevant variables
blut[clinical.course.variables$Variable.Name[clinical.course.variables$Data == "blut"]] <- 
  scale(blut[clinical.course.variables$Variable.Name[clinical.course.variables$Data == "blut"]])
blut$CK[position] <- text

bga <- read.csv("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/A_Bga_Werte.csv") %>%
  mutate(DtTm = as.POSIXlt(Messung_dttm, format = "%Y-%m-%d %H:%M:%S") )

# Scale relevant variables
bga[clinical.course.variables$Variable.Name[clinical.course.variables$Data == "bga"]] <- 
  scale(bga[clinical.course.variables$Variable.Name[clinical.course.variables$Data == "bga"]])

vitals <- read.csv("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/R_Vital_Werte_Wide.csv") %>%
  mutate(DtTm = as.POSIXlt(STOPDAT, format = "%Y-%m-%d %H:%M:%S") ) %>%
  mutate(MAP = (2*Diastolic + Systolic)/3)
                                        
# Scale relevant variables
vitals[clinical.course.variables$Variable.Name[clinical.course.variables$Data == "vitals"]] <- 
  scale(vitals[clinical.course.variables$Variable.Name[clinical.course.variables$Data == "vitals"]])

# Run the Functions
outcome.vitals <- matrix(ncol = nrow(clinical.course.variables), nrow = nrow(patients))
colnames(outcome.vitals) <- clinical.course.variables$Variable.Name
for(i in 1:nrow(patients)){
  outcome.vitals[i,] <- ClinicalCourse.outcome(patients$research_case_id[i], patients$DtTmStop[i], clinical.course.variables, operations)
}
save(outcome.vitals, file = "/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/Outcome.Vitals.RData")
outcome.final <- weight.ClinicalCourse.outcome(outcome.vitals, clinical.course.variables$weights)

hist(outcome.final[outcome.final<10], breaks = "FD", main = "Weighted Mean")
abline(v = 0, lty = 2, col = "red", lwd = 2)
text(-1, 200, expression(y[i] == 0))
text(1, 200, expression(y[i] == 1))

# Check Outcomes of the clinical course variables

# turn the variable values to numeric
suppressWarnings(
outcome.vitals <- as_tibble(outcome.vitals) %>%
  mutate_all(as.numeric))

# histograms of each variable
par(mfrow = c(3, 1), mar=rep(2, 4))
hist(outcome.vitals$Lac, breaks = "FD", main = "Lactate Score", xlab = "Trend")
hist(outcome.vitals$Temperatur, breaks = "FD", main = "Temperature Score", xlab = "Trend")
hist(outcome.vitals$Puls, breaks = "FD", main = "Puls Score", xlab = "Trend")

# How many surgeries without any post-surgery vital information available
table(apply(outcome.vitals, 1, function(x) sum(is.na(x)) == 32))

# Distribution of the available variable compositions
vital.comp <- apply(outcome.vitals, 1, function(x) paste(colnames(outcome.vitals)[!is.na(x)], collapse = ' ')) %>%
  as_tibble() %>%
  group_by(value) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  write.csv("/Users/Matthias/Desktop/ClinicalCourseVariableComposition.csv")

# Share of missings in % per variable
sort(apply(outcome.vitals, 2, function(x) sum(is.na(x))/length(x)))

# How many post-surgery variables are available per surgery
hist(apply(outcome.vitals, 1, function(x) sum(!is.na(x))), breaks = 30,
     main = "Number of Clinical Course Variables Development Available",
     xlab = "Number of Variables")

# Average of the average number of variables
mean(apply(outcome.vitals, 1, function(x) sum(!is.na(x))/length(x)))

# Add labels to the patient file
for(i in 1:nrow(patients)){
  # only replace "check vitals" flags
  if (patients$flag[i] != "check vitals") {next}
  vital.values <- ClinicalCourse.outcome(patients$research_case_id[i], patients$DtTmStop[i], clinical.course.variables, operations)
  patients$flag[i] <- weight.ClinicalCourse.outcome(vital.values, clinical.course.variables$weights)
  if (i %% 100 != 0){cat(".")}
  else{cat(i, "\n")}
}

save(patients, file ="/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/V_Outcome_Y_Coded.Rdata")
