library(dplyr)
# Reading in data - cleaned data with fewer NA's if appropriate
vitals <- read.csv("/Users/Taru/Desktop/Dataset/[SDS].[v_Export_Vital_Werte_tra_sds].csv", 
                   stringsAsFactors = TRUE) 

vitals <- vitals[, c("research_case_id", "research_id",
                     "Messung_Name", "Messung_Wert",
                     "STARTDAT", "STOPDAT")]

vitals[["STARTDAT"]] <- as.POSIXct(vitals[["STARTDAT"]], 
                                   tz = "Europe/Zurich", 
                                   format = "%Y-%m-%d %H:%M:%OS")

vitals[["STOPDAT"]] <- as.POSIXct(vitals[["STOPDAT"]], 
                                  tz = "Europe/Zurich", 
                                  format = "%Y-%m-%d %H:%M:%OS")
vitals <- distinct(vitals) 
# systolic/diastolic

#levels(vitals$Messung_Name)

levels(vitals$Messung_Name) <- c(levels(vitals$Messung_Name), 
                                 "Systolic", "Diastolic")


systolic <- vitals[which(vitals[["Messung_Name"]]=="Blutdruck") , ] # 3745: -/-

systolic$Messung_Name <- "Systolic"
systolic$Messung_Wert <- sapply(strsplit(as.character(systolic$Messung_Wert),"/"),
                                function(x) {as.numeric(x[1])})

diastolic <- vitals[which(vitals[["Messung_Name"]]=="Blutdruck") , ]
diastolic$Messung_Name <- "Diastolic"
diastolic$Messung_Wert <- sapply(strsplit(as.character(diastolic$Messung_Wert),"/"),
                                 function(x) {as.numeric(x[2])})

vitals <- vitals[-which(vitals[["Messung_Name"]]=="Blutdruck") , ]
vitals <- vitals[-which(vitals[["Messung_Name"]]=="Temperatur rectal") , ]

vitals[["Messung_Wert"]] <- as.numeric(as.character(vitals[["Messung_Wert"]]))

vitals <- rbind(vitals, systolic, diastolic)
head(vitals)
str(vitals)

vitals <- arrange(vitals, research_case_id, STARTDAT, Messung_Name)
head(vitals)
summary(vitals)

# replace 0 with NA

vitals$Messung_Wert[vitals$Messung_Wert == 0] <- NA

summary(vitals)

mapping <- read.csv('/Users/Taru/desktop/Dataset/Mapping (all trauma).csv',
                    stringsAsFactors = T) # only severe trauma here!!

case_id <- case_id <- mapping %>%
  select(research_case_id) %>%
  unique() %>%
  unlist()



filter_dataset <- function(data, ids = case_id){
  
  filtered <- data %>%
    filter(research_case_id %in% ids) %>%
    as_tibble()
  
  return(filtered)
  
}


vitals_filtered <- filter_dataset(vitals)


write.csv(vitals_filtered, "/Users/Taru/Desktop/vitals_long.csv")
