# Find subsequent surgery. If multiple surgeries, return the date time of the subsequent surgery

find.previous.surgery <- function(id, startdat, operations.data){
  
  startdat <- as.POSIXlt(startdat, format = "%Y-%m-%d %H:%M:%S")
  # only keep
  operations.data <- operations.data %>%
    # relevant cases
    filter(research_case_id == id) %>%
    # surgeries with stopdat == stopdat or later
    # if multiple surgeries but only 1 obs left, it was the last surgery
    filter(DtTmStart <= startdat) %>%
    arrange(desc(DtTmStart))
  
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

# Find ICU Vitals

icu.outcome <- function(id, surgery.time, icu.data, operations.data){
  # get subsequent surgery (if available)
  previous.surgery <- find.previous.surgery(id, surgery.time, operations.data)
  
  # filter data by
  icu.data <- icu.data %>%
    filter(research_case_id == id) # relevant patient

  # If single surgery
  if (is.na(previous.surgery)){
    # filter data by
    icu.data <- icu.data %>%
      filter(DtTm <= surgery.time) %>% # only vitals after surgery
      arrange(DtTm)
  }
  
  # If multiple surgeries
  else{
    # filter data by
    icu.data <- icu.data %>%
      filter(DtTm <= surgery.time & DtTm > previous.surgery) %>% # only vitals in between surgeries
      arrange(DtTm)
  }
  
  firstt <- function(data, na.rm = TRUE){
    return(first(data))
  }
  
  lastt<- function(data, na.rm = TRUE){
    last_elem <- data[length(data)]
    if (length(last_elem) > 0){
      return(last_elem)  
    }
    else{return(NA)}
  }
  
  kurtosiss <- function(data, na.rm = TRUE){
    if (sum(is.na(data))==1){
      return(0)
    }
    else {return(kurtosis(data, na.rm = TRUE))}
  }
  
  skewnesss <- function(data, na.rm = TRUE){
    if (sum(is.na(data))==1){
      return(0)
    }
    else {return(skewness(data, na.rm = TRUE))}
  }
  
  sdd <- function(data, na.rm = TRUE){
    if (sum(is.na(data))==1){
      return(0)
    }
    else {return(sd(data, na.rm = TRUE))}
  }
  
  # variables to extract features
  features.normal <- vars(Herzfrequenz, Atemfrequenz, SpO2, FiO2, ArterialBP)
  functions <- funs(max, min, median, mean, sd = sdd, 
                    n = sum(!is.na(.)), first = firstt(na.omit(.)), last = lastt(na.omit(.)),
                    lq = quantile(., prob = 0.25), uq = quantile(., prob = 0.75), iqr = IQR(.),
                    skew = skewnesss(.), kurt = kurtosiss(.))
  features <- icu.data %>%
    summarise_at(features.normal, functions, na.rm = TRUE)
  
  # variables to check if measured
  features.exist <- vars(Groesse, Temperatur, Gewicht, Verbale, Motorische, Augen)
  features <- icu.data %>%
    summarise_at(features.exist, funs(exist = any(!is.na(.))), na.rm = TRUE) %>%
    bind_cols(features)
  
  # return how many vitals
  return(features)
  
}

# Load Packages and Directory
library(e1071)

# Set directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load Data
operations <- read.csv("./../Data/SurgeryOutcome/H_Operationen.csv") %>%
  mutate(DtTmStop = as.POSIXlt(Stopdat, format = "%Y-%m-%d %H:%M:%S") ) %>%
  mutate(DtTmStart = as.POSIXlt(Startdat, format = "%Y-%m-%d %H:%M:%S") )

# Load Operations with labels
load("./../Data/SurgeryOutcome/V_Outcome_Y_Coded.Rdata")
patients <- patients %>% dplyr::filter(flag == "0" | flag == "1") %>%
  filter(DtTmStart >= as.POSIXlt("2017-01-01", format = "%Y-%m-%d"))

# load ICU Data
load("./../Data/ICU_params_final.RData")
names(ICU_param_wide) <- c("research_case_id",
                           "DtTm",
                           "Herzfrequenz",
                           "Atemfrequenz",
                           "SpO2",
                           "FiO2",
                           "Atemfrequenz2",
                           "Groesse",
                           "ArterialBP",
                           "Temperatur",
                           "Punkte",
                           "Gewicht",
                           "Verbale",
                           "Motorische",
                           "Augen",
                           "max_AIS")
ICU_param_wide <- ICU_param_wide %>%
  dplyr::filter(research_case_id %in% patients$research_case_id) %>%
  # combine the two atemfrequenzen:
  # only Atemfrequenz2 available
  mutate(Atemfrequenz = ifelse(is.na(Atemfrequenz) & !is.na(Atemfrequenz2), Atemfrequenz2, Atemfrequenz)) %>%
  # both available
  mutate(Atemfrequenz = ifelse(!is.na(Atemfrequenz) & !is.na(Atemfrequenz2), mean(c(Atemfrequenz, Atemfrequenz2)), Atemfrequenz)) %>%
  # make non.sense measurements to NA
  mutate(ArterialBP = ifelse(ArterialBP > 0, ArterialBP, NA)) %>%
  dplyr::select(-c(max_AIS, Atemfrequenz2))

constant.features <- read.csv("./../Data/SurgeryOutcome/U_Single_Measurements_Patient.csv")

# Run the Functions

temp <- NULL
for (i in 1:nrow(patients)){
    temp <- temp %>% bind_rows(icu.outcome(patients$research_case_id[i], patients$DtTmStart[i], ICU_param_wide, operations))
}
icu.features <- temp

# make inf and Nan to NA
for (i in 1:ncol(icu.features)){
  icu.features[is.infinite(icu.features[[i]]), i] <- NA 
  icu.features[is.nan(icu.features[[i]]), i] <- NA 
}

# add labels:
icu.features <- cbind(patients[, c("flag", "research_case_id", "DtTmStart", "DtTmStop", "multiple")], outcome.vitals)

# add constant variables
icu.features %>%
  left_join(constant.features, by = research_case_id)

save(icu.features, file = "./../Data/W_ICU_Features.Rdata")

# Check Data Availability

apply(outcome.vitals, 2, function(x) x[is.infinite(x)]=NA)

# How many surgeries without any post-surgery vital information available
table(apply(outcome.vitals, 1, function(x) sum(is.na(x)) >= 10))

# Share of missings in % per variable
sort(apply(outcome.vitals, 2, function(x) sort(sum(is.na(x))/length(x))))

# How many post-surgery variables are available per surgery
hist(apply(outcome.vitals, 1, function(x) sum(!is.na(x))), breaks = 30,
     main = "Number of Clinical Course Variables Development Available",
     xlab = "Number of Variables")

# Average of the average number of variables
mean(apply(outcome.vitals, 1, function(x) sum(!is.na(x))/length(x)))
