
# This script identifies the outcome of definitive surgeries.
# It compares pre- and post-outcomes of selected variables and then
# Builds decisions based on certain rules

# Updated: 5th May 2021

##### Functions #####

# F: Surgery time and date of given research case id and treatment.time----
# Note that treatment time is the time indicated in Sascha's DCO_Defsurg.xlsx file

find.surgery.time <- function(id, treatment.time = NA, operations.data){

  operations.data <- operations.data %>%
    filter(research_case_id == id)
  
  # check if id is in operations data
  if (nrow(operations.data) == 0){
    return(NA)
  }
  # check if unique surgery and treatment time provided
  if (nrow(operations.data) > 1 & !is.na(treatment.time)){
    # if multiple surgeries find the one closest to treatment.time
    treatment.time <- as.POSIXlt(treatment.time, format = "%Y-%m-%d %H:%M:%S")
    operations.data <- operations.data %>%
      mutate(time.delta = DtTm - treatment.time) %>%
      filter(time.delta == min(time.delta))
  }
  relevant_date <- operations.data$DtTm[1]
  return(relevant_date)
}

# F: [Not Used] find h-hours time window before/after t hours of surgery----

find.time.window <- function(t, h, id){
  # get surgery time
  surgery.time <- find.surgery.time(id)
  # t hours post/pre surgery time
  delta.surgery.time <- survery.time + 60*60*t # additions in secs
  # h hours window
  delta.surgery.window <- delta.surgery.time + c(-1, 1)*60*60*h/2
  
  return(delta.surgery.window)
}

# F: [Not Used] h hours after surgery vital for descriptive----
# this function is used for summary statistics and shows where and why data is missing
# note that datatime variable of data needs to be POSIXlt and in a variable named DtTm


find.vitals.descriptive <- function(h, id, data, vital){
  
  # get surgery time
  find.surgery <- find.surgery.time(id)
  
  # exit if no surgery time
  if (is.na(find.surgery)){
    return("No Surgery Time")
  }
  
  #get time h hours after surgery
  after.surgery <- find.surgery+60*60*h # addition is secs
  
  # filter data by relevant patient
  data <- data %>%
    filter(research_case_id == id) # ...relevant patient
  
  # exit if no obs left
  if (nrow(data) == 0){
    return("Patient not in data")
  }
  
  # filter data by missing values
  data <- data %>%
    filter(vital != "" | !is.na(vital)) # ...non-missing vital
  
  # exit if no obs left
  if (nrow(data) == 0){
    return("Vital only with missing")
  }
  
  # filter data by missing values
  data <- data %>%
    filter(DtTm >= after.surgery) # ...relevant patient
  
  # exit if no obs left
  if (nrow(data) == 0){
    return("No obs. in filtered time window")
  }
  
  data <- data[vital]
  # return the number of obs
  return(nrow(data))
}

# F: [Not Used] Error counter for descriptive----

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

# F: h hours after surgery vital values----
# this function returns whether a provided vital value developed positively (= 1) or negatively (= 0)
# note that datatime variable of data needs to be POSIXlt and in a variable named DtTm

temp2 <- numeric(nrow(operations.new))
for (i in 1:nrow(operations.new)){
  temp2[i] <- vital.outcome(0, operations.new$research_case_id[i], blut, "BAS", operations.new$DtTm[i], operations)
}

vital.outcome <- function(h, id, data, vital, treatment.time = NA, operations.data){

  # get surgery time
  surgery.time <- find.surgery.time(id, treatment.time, operations.data)
  
  # exit if no surgery time
  if (is.na(surgery.time)){
    return(2)
  }
  
  # filter data by...
  data <- data %>%
    filter(research_case_id == id) # ...relevant patient
    
  # exit if no obs left
  if (nrow(data) == 0){
    return(3)
  }
  
  # filter data by...
  data <- data %>%
    filter(get(vital) != "" | !is.na(get(vital))) # ...no missing values
    
  # exit if no obs left
  if (nrow(data) == 0){
    return(4)
  }
  
  # filter data by...
  data <- data %>%
    filter(DtTm >= surgery.time) # ...only vitals h hours after surgery

  # exit if no obs left
  if (nrow(data) == 0){
    return(5)
  }
  
  # filter data by...
  data <- data %>%
    filter(!is.na(suppressWarnings(as.numeric(get(vital))))) # ...only vitals that are numbers
  
  # exit if no obs left
  if (nrow(data) == 0){
    return(6)
  }
  
  # code as 1 if only one obs after surgery ()
  if (nrow(data) == 1){
    return(1)
  }
  
  # trend between 1st and 2nd obs
  if (nrow(data) > 1){
    data <- data %>% 
      arrange(DtTm)
    trend <- as.numeric(data[2,vital]) - as.numeric(data[1,vital])
    # variables for which a increase is postive
    increase <- c("CKDEPI", "NEUA", "FBG", "QUICK", "APTT", "T", "Temperatur", "GCS", "po2", "FIO2")
    # variables for which a decrease is positive
    decrease <- c("Lac", "BAS", "BASA", "CRP", "IMGR", "IMGRA", "LC",
                  "LYM", "NEU", "PCT", "PCTB", "TC", "GGT", "KHINR",
                  "PTZEIT", "TZI", "HUFH", "CK", "LDH", "MYO")
    if (sign(trend) == 1 & vital %in% increase){return(1)}
    if (sign(trend) == -1 & vital %in% decrease){return(1)}
    else{return(0)}
  }
  
}

# F: Clinical Course Hemorrhage----
# returns positive / negative outcome for Hemorrhage of clinical courses from https://polytraumacourse.com/clinical-course-12-h
clinical.course.variables <- read.csv2("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/ClinicalCourseVariables.csv")
hemorrhage.vars <- clinical.course.variables %>%
  filter(Clinical.Course.Class != "Hemorrhage")

ClinicalCourse.outcome <- function(id, Clinical.Cours.vars, treatment.time = NA, operations.data) {

  n.vars <- nrow(Clinical.Cours.vars)
  vital.outcomes <- numeric(n.vars)
  names(vital.outcomes) <- Clinical.Cours.vars$Variable.Name
  for (i in 1:n.vars){
    vital <- Clinical.Cours.vars$Variable.Name[i]
    vitalDataset <- Clinical.Cours.vars$Data[i]
    vital.outcomes[i] <- vital.outcome(0, id, get(vitalDataset), vital, treatment.time, operations.data) 
  }
  return(vital.outcomes)
}

# Using the list of surgeries from Sascha

temp <- matrix(nrow = nrow(operations.new), ncol = nrow(clinical.course.variables))
for (i in 1:nrow(operations.new)){
  temp[i,] <- ClinicalCourse.outcome(operations.new$research_case_id[i], clinical.course.variables, operations.new$DtTm[i], operations.data = operations)
}

# How many surgeries without any post-surgery value available
table(apply(temp, 1, function(x) ifelse(1 %in% x | 0 %in% x, 1, 0)))
# Reasons for missing
table(temp)
# How many post-surgery variables are available per surgery
hist(apply(temp, 1, function(x) sum(x == 0 | x == 1)), breaks = 30)
# Average of the average number of variables
mean(apply(temp, 1, function(x) sum(x == 0 | x == 1)/length(x)))

# Using the list of patients from Taru

temp2 <- matrix(nrow = nrow(patients), ncol = nrow(clinical.course.variables))
for (i in 1:nrow(patients)){
  temp2[i,] <- ClinicalCourse.outcome(patients$research_case_id[i], clinical.course.variables, operations.data = operations)
  if (i %% 100 == 0){cat("=")}
}

# How many surgeries without any post-surgery value available
table(apply(temp2, 1, function(x) ifelse(1 %in% x | 0 %in% x, 1, 0)))
# How many post-surgery variables are available per surgery
hist(apply(temp2, 1, function(x) sum(x == 0 | x == 1)), breaks = 30)
# Average of the average number of variables
mean(apply(temp2, 1, function(x) sum(x == 0 | x == 1)/length(x)))

table(temp2)

##### Work #####

# Load Relevant Patients and all Operations----

patients <- read.csv("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/single_surgery_definitive_ids.csv")
operations <- read.csv("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/Operationen_raw.csv") %>%
  mutate(DtTm = as.POSIXlt(Stopdat, format = "%Y-%m-%d %H:%M:%S") )
operations.new <- read.csv2("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/DCO_Defsurg.csv") %>%
  dplyr::filter(Intervention == "DefSurg") %>%
  mutate(DtTm = as.POSIXlt(Therapy_dat, format = "%Y-%m-%d %H:%M:%S") ) %>%
  dplyr::select(research_case_id, DtTm)
  
# Load the data-----

blut <- read.csv("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/Blut_Werte.csv")
blut$DtTm <- as.POSIXlt(blut$ObservationDtTm, format = "%Y-%m-%d %H:%M:%S") 

bga <- read.csv("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/Bga_Werte.csv")
bga$DtTm <- as.POSIXlt(bga$Messung_dttm, format = "%Y-%m-%d %H:%M:%S") 

vital <- read.csv("/Users/Matthias/Library/Mobile Documents/com~apple~CloudDocs/ETH/Stats Lab/Data/SurgeryOutcome/Vital_Werte_long.csv")
vital <- vital[!duplicated(vital[,c(1,3,5)], fromLast = FALSE),]
vital <- pivot_wider(vital, names_from = Messung_Name,
              values_from = c(Messung_Wert))
vital$DtTm <- as.POSIXlt(vital$STARTDAT, format = "%Y-%m-%d %H:%M:%S") 

# Check for missings-----

matrix.col.names <- c("Average obs. h hours after surgery",
                           "Patient not in data",
                           "No obs. in filtered time window",
                           "Vital only with missing",
                           "No Surgery Time")

blut.vars <- c("CKDEPI", "BAS", "BASA", "CRP", "IMGR", "IMGRA", 
               "LC", "LYM", "NEU", "NEUA", "PCT", "PCTB", "TC", 
               "FBG", "GGT", "KHINR", "PTZEIT", "QUICK", "TZI", 
               "HUFH", "CK", "LDH", "MYO")

blut.missings <- matrix(nrow = length(blut.vars), ncol = 5)
rownames(blut.missings) <- blut.vars
colnames(blut.missings) <- matrix.col.names
for (var in blut.vars){
  blut.missings[var,] <- error.counter(sapply(patients[,1], function(id) find.vitals.descriptive(0, id, blut, var)))
}

vital.vars <- c("Temperatur", "GCS")
vital.missings <- matrix(nrow = length(vital.vars), ncol = 5)
rownames(vital.missings) <- vital.vars
colnames(vital.missings) <- matrix.col.names
for (var in vital.vars){
  vital.missings[var,] <- error.counter(sapply(patients[,1], function(id) find.vitals.descriptive(0, id, vital, var)))
}

bga.vars <- c("Lac", "T", "pO2", "FIO2")
bga.missings <- matrix(nrow = length(bga.vars), ncol = 5)
rownames(bga.missings) <- bga.vars
colnames(bga.missings) <- matrix.col.names
for (var in bga.vars){
  bga.missings[var,] <- error.counter(sapply(patients[,1], function(id) find.vitals.descriptive(12, id, bga.orig, var)))
}

# Check for Vital Values-----

for (var in blut.vars){
  blut.missings[var,] <- error.counter(sapply(patients[,1], function(id) find.vitals.descriptive(0, id, blut, var)))
}
