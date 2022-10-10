### StatsLab ### Data Cleaning ###

## Packages
library(dplyr)
library(fasttime)

## Functions
# Converting empty strings to NAs
empty2NA <- function(x) {
  for (i in 1:length(x)) {
    if (is.character(x[, i])) {
      x[, i] <- gsub("^$|^ $", NA, x[, i])
    }
  }
  x
}

# Function for making a column numeric and removing descriptions
numer_fun <- function(x) {
  x1 <- gsub("[a-z]", NA, x)
  x2 <- as.numeric(x1)
  x2
}

## Loading the original data
filenames <- list.files("data", pattern="*.csv", full.names=TRUE)
for (i in 1:length(filenames)) assign(filenames[i], read.csv(filenames[i]))

## Assigning the data to variables (to have meaningful variable names)
A_Bga_Werte <- `data/[SDS].[v_Export_Bga_Werte_tra_sds].csv`
B_Blut_Werte <- `data/[SDS].[v_Export_Blut_Werte_tra_sds].csv`
C_Diagnosen <- `data/[SDS].[v_Export_Diagnosen_tra_sds].csv`
D_Komplikationen <- `data/[SDS].[v_Export_Komplikationen_tra_sds].csv`
E_Mapping_AIS <- `data/[SDS].[v_Export_Mapping_Diagnosen_ICD_to_AIS_tra_sds].csv`
F_Medikamente <- `data/[SDS].[v_Export_Medikamente_tra_sds].csv`
G_Mikrobiol <- `data/[SDS].[v_Export_Mikrobiol_Werte_tra_sds].csv`
H_Operationen <- `data/[SDS].[v_Export_Operationen_Proc_tra_sds].csv`
I_Patient_Unfall <- `data/[SDS].[v_Export_Patient_Unfall_SpitalAufenthalt_tra_sds].csv`
J_PDMS_Mech_Beatmung <- `data/[SDS].[v_Export_PDMS_Mechanische_Beatmung_Dauer_sds].csv`
K_PDMS_Parameter <- `data/[SDS].[v_Export_PDMS_Parameters_sds].csv`
L_Rotem_Werte <- `data/[SDS].[v_Export_Rotem_Werte_tra_sds].csv`
M_Score_ISS_from_AIS <- `data/[SDS].[v_Export_Score_ISS_from_AIS_berechnet_tra_sds].csv`
N_Score_Saps <- `data/[SDS].[v_Export_Score_Saps_tra_sds].csv`
O_Score_Ips <- `data/[SDS].[v_Export_Scores_Ips_tra_sds].csv`
P_Stat_Teilstat_Faelle <- `data/[SDS].[v_Export_Stat_Teilstat_Faelle_tra_sds].csv`
Q_Untersuchungen <- `data/[SDS].[v_Export_Untersuchungen_tra_sds].csv`
R_Vital_Werte <- `data/[SDS].[v_Export_Vital_Werte_tra_sds].csv`

## Removing the unnecessary variables
rm(list = ls()[grep("data/", ls())])

## Loading Adnana's Data
filenames2 <- list.files("New Data/Adnana", pattern="*.csv", full.names=TRUE)
for (i in 1:length(filenames2)) assign(filenames2[i], read.csv(filenames2[i]))
A_Bga_Werte <- `New Data/Adnana/bga_values_clean.csv`
B_Blut_Werte <- `New Data/Adnana/blut_werte_clean.csv`
D_Komplikationen <- `New Data/Adnana/complications_clean.csv`
Q_Untersuchungen <- `New Data/Adnana/investigation.csv`
N_Score_Saps <- `New Data/Adnana/SAPS.csv`
R_Vital_Werte <- `New Data/Adnana/vitals.csv`
rm(list = ls()[grep("New Data/", ls())])





##### 1 ##### SIMPLE CLEANING #####

## A_Bga_Werte
A_Bga_Werte <- A_Bga_Werte[, -1]
A_Bga_Werte[, "Messung_dttm"] <- as.POSIXct(A_Bga_Werte[, "Messung_dttm"], format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich")

# Column spezieller.Probentyp
A_Bga_Werte$spezieller.Probentyp <- gsub("^$|^ $", "Nicht spezifiziert", A_Bga_Werte$spezieller.Probentyp)
Spez <- unique(A_Bga_Werte[grep("spezi", A_Bga_Werte$spezieller.Probentyp), 44])
for (i in 1:length(A_Bga_Werte[, 44])){
  if (A_Bga_Werte[i, 44] %in% Spez) {
    A_Bga_Werte[i, 44] <- "Nicht spezifiziert"
  }
}

# Column CRP
A_Bga_Werte$CRP <- gsub("^$|^ $", NA, A_Bga_Werte$CRP)
A_Bga_Werte$CRP <- gsub("<5", 0, A_Bga_Werte$CRP)
A_Bga_Werte$CRP <- as.numeric(A_Bga_Werte$CRP)

# Removing extreme values
A_Bga_Werte[64665, c(4, 8, 13, 14, 18, 20, 30)] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 5] < -1e03), 5] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 8] < -100), 8] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 12] > 2000), 12] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 13] > 100), 13] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 13] < 0), 13] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 14] < -500), 14] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 16] < -100), 16] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 19] > 100), 19] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 21] == -1000), 21] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 23] == -1000), 23] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 23] == 1000), 23] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 27] < -1e03), 27] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 28] < 32), 28] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 28] > 45), 28] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 33] < 0), 33] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 36] < 0), 36] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 37] < 0), 37] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 38] < 0), 38] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 40] == 1000), 40] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 40] == -1000), 40] <- NA
A_Bga_Werte[which(A_Bga_Werte[, 42] < -100), 42] <- NA


## B_Blut_Werte
# Converting empty strings to NAs
B_Blut_Werte <- empty2NA(B_Blut_Werte)

# Converting the date to a date format
B_Blut_Werte$ObservationDtTm <- as.POSIXct(B_Blut_Werte$ObservationDtTm, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich")

# In the following, the focus lays on the variables listed in Matthias' Excel
# Making numeric variables numeric
nm <- c("BAS", "BASA", "CRP", "IMGR", "IMGRA", "LC", "LYM", "NEU", "NEUA", "PCT", "PCTB", "TC", "GGT", "PTZEIT", "HUFH", "LDH")
nm_nb <- which(names(B_Blut_Werte) %in% nm)

for (i in nm_nb) {
  B_Blut_Werte[, i] <- numer_fun(B_Blut_Werte[, i])
}

# Categorization of variables
# CKDEPI # Categorical
g <- B_Blut_Werte$CKDEPI
g1 <- gsub(">90", 91, g)
g1 <- gsub("[a-z]", NA, g1)
g2 <- as.numeric(g1)

g3 <-
  cut(
    g2,
    breaks = c(0, seq(10, 90, by = 10), Inf),
    include.lowest = T,
    ordered_result = TRUE
  )
plot(g3)

B_Blut_Werte$CKDEPI_String <- B_Blut_Werte$CKDEPI
B_Blut_Werte$CKDEPI <- g2
B_Blut_Werte$CKDEPI_Categ <- g3

# FBG # Categorical
g <- B_Blut_Werte$FBG
g1 <- gsub("[a-z]", NA, g)
g1 <- gsub(">9.0", 9, g1)
g1 <- gsub("<0.5", 0, g1)
g2 <- as.numeric(g1)

g3 <-
  cut(
    g2,
    breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, Inf),
    include.lowest = T,
    ordered_result = TRUE
  )
plot(g3)

B_Blut_Werte$FBG_String <- B_Blut_Werte$FBG
B_Blut_Werte$FBG <- g2
B_Blut_Werte$FBG_Categ <- g3

# KHINR # Categorical
g <- B_Blut_Werte$KHINR
g1 <- gsub("[a-z]", NA, g)
g1 <- gsub("> 4.9", 4.9, g1)
g1 <- gsub("> 4.4", 4.4, g1)
g2 <- as.numeric(g1)
g2[which(g2 == 43)] <- NA
unique(g2)

g3 <-
  cut(
    g2,
    breaks = c(0, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 3.75, 4, 4.25, Inf),
    include.lowest = T,
    ordered_result = TRUE
  )
plot(g3)

B_Blut_Werte$KHINR_String <- B_Blut_Werte$KHINR
B_Blut_Werte$KHINR <- g2
B_Blut_Werte$KHINR_Categ <- g3

# QUICK # Categorical
g <- B_Blut_Werte$QUICK
g1 <- gsub("[a-z]", NA, g)
g1 <- sub("< 10, < 10", 0, g1)
g1 <- sub("< 10", 0, g1)
g1 <- sub(">>130", 140, g1)
g1 <- sub(">120, >120", 140, g1)
g1 <- sub(">127", 140, g1)
g1 <- sub(">138", 140, g1)
g1 <- sub(">120", 140, g1)
g2 <- as.numeric(g1)

g3 <-
  cut(
    g2,
    breaks = c(seq(0, 120, by = 5), Inf),
    include.lowest = T,
    ordered_result = TRUE
  )
plot(g3)

B_Blut_Werte$QUICK_String <- B_Blut_Werte$QUICK
B_Blut_Werte$QUICK <- g2
B_Blut_Werte$QUICK_Categ <- g3

# TZI # Categorical
g <- B_Blut_Werte$TZI
g1 <- gsub("[a-z]", NA, g)
g1 <- sub(">120, >120", 200, g1)
g1 <- sub(">120", 200, g1)
g1 <- sub(">200", 200, g1)
g2 <- as.numeric(g1)

g3 <-
  cut(
    g2,
    breaks = c(0, seq(10, 40), Inf),
    include.lowest = T,
    ordered_result = TRUE
  )
plot(g3)

B_Blut_Werte$TZI_String <- B_Blut_Werte$TZI
B_Blut_Werte$TZI <- g2
B_Blut_Werte$TZI_Categ <- g3

# APTT # Categorical
g <- B_Blut_Werte$APTT
g1 <- gsub("[a-z]", NA, g)
g1 <- gsub(">160", 160, g1)
g2 <- as.numeric(g1)

g3 <-
  cut(
    g2,
    breaks = c(0, seq(16, 80, by = 2), Inf),
    include.lowest = T,
    ordered_result = TRUE
  )
plot(g3)

B_Blut_Werte$APTT_String <- B_Blut_Werte$APTT
B_Blut_Werte$APTT <- g2
B_Blut_Werte$APTT_Categ <- g3

# CK # Numeric (Special)
g <- B_Blut_Werte$CK
g1 <- gsub("[a-z]", NA, g)
g1 <- gsub("<7", 0, g1)
g2 <- as.numeric(g1)
B_Blut_Werte$CK <- g2

# MYO # Categorical
g <- B_Blut_Werte$MYO
g1 <- gsub("[a-z]", NA, g)
g1 <- gsub(">30000", 30000, g1)
g1 <- gsub("<21", 0, g1)
g2 <- as.numeric(g1)

g3 <-
  cut(
    g2,
    breaks = c(0, seq(20, 2000, by = 20), Inf),
    include.lowest = T,
    ordered_result = TRUE
  )
plot(g3)

B_Blut_Werte$MYO_String <- B_Blut_Werte$MYO
B_Blut_Werte$MYO <- g2
B_Blut_Werte$MYO_Categ <- g3

# Aggregating
B_Blut_Werte_Categorized <-
  subset(B_Blut_Werte,
    select = c(2:36, 129:130, 37:48, 131, 49, 119:120, 50:56, 121:122, 57:74, 123:124, 75:94, 132:133, 95:113, 
               125:126, 114:117, 127:128, 118))
rm(B_Blut_Werte, g, g1, g2, g3)


## C_Diagnosen
names(C_Diagnosen)[1] <- "Code"
C_Diagnosen$Dat <- as.POSIXct(C_Diagnosen$Dat, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich")
# rm(C_Diagnosen) # should be excluded according to the Data Guide


## D_Komplikationen
D_Komplikationen <- D_Komplikationen[, -1]
# rm(D_Komplikationen) # should be excluded according to the Data Guide

## E_Mapping_AIS
# Finding out which research case IDs are linked to a trauma with an AIS score of at least 3
x <- unique(E_Mapping_AIS$research_case_id)
k <- NULL
for (i in x) {
  y <- E_Mapping_AIS[E_Mapping_AIS$research_case_id == i, ]
  z <- length(y[y$MAX_AIS_SEVERITY >= 3, 9])
  if (z != 0) {k <- c(k, i)}
}

# Filtering the data
u <- rep(NA, dim(E_Mapping_AIS)[1])
for (i in 1:length(E_Mapping_AIS$research_case_id)) {
  u[i] <- any(E_Mapping_AIS[i, 11] %in% k)
}
E_Mapping_AIS <- E_Mapping_AIS[u, ]
E_Mapping_AIS <- E_Mapping_AIS[, -c(4, 5, 6, 8, 10, 13)]
E_Mapping_AIS$Dat <- as.POSIXct(E_Mapping_AIS$Dat, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich")


## F_Medikamente
names(F_Medikamente)[1] <- "Medikament_name"
F_Medikamente <- F_Medikamente[grep("Norepinephrin", F_Medikamente$Medikament_name), ]
F_Medikamente[, c(4, 5)] <-
  lapply(F_Medikamente[, c(4, 5)], function(i)
    as.POSIXct(i, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich"))
F_Medikamente <- F_Medikamente[, -c(6, 7)]


## G_Mikrobiol
# rm(G_Mikrobiol) # should be excluded according to the Data Guide


## H_Operationen
names(H_Operationen)[1] <- "abgabedttm"

# Converting empty strings to NAs
H_Operationen <- empty2NA(H_Operationen)

H_Operationen[, c(1, 2, 4, 5, 6, 7, 9, 10, 11, 13, 14, 18, 19, 20)] <-
  lapply(H_Operationen[, c(1, 2, 4, 5, 6, 7, 9, 10, 11, 13, 14, 18, 19, 20)], function(i)
    as.POSIXct(i, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich"))

# From here on onwards to the end of the code that is related to the creation of H_Operationen (i.e. lines 338-380), the aggregation of surgeries is carried out
H_Operationen <- H_Operationen %>%
  mutate(DtTmStop = as.POSIXlt(Stopdat, format = "%Y-%m-%d %H:%M:%S") ) %>%
  mutate(DtTmStart = as.POSIXlt(Startdat, format = "%Y-%m-%d %H:%M:%S") )

H_Operationen <- H_Operationen %>%
  group_by(research_case_id) %>%
  # earliest date first
  arrange(DtTmStop, .by_group = TRUE) %>%
  # time delta to the NEXT surgery by hours
  mutate(surgery.time.delta = (lead(DtTmStart, 1) - DtTmStop)/3600, .after = Stopdat) %>%
  # set the time delta "high" for the last obs of a case id (to avoid NA)
  mutate(surgery.time.delta = ifelse(is.na(surgery.time.delta), 100, surgery.time.delta)) %>%
  # count within group (we see each row as one surgery)
  mutate(SurgeryNr = row_number(), .after = surgery.time.delta)

# now we identify which rows belong to the same surgery
for (i in 2:nrow(H_Operationen)) {
  # if time delta of the PREVIOUS surgery is less than 6h and the same research case id
  if (H_Operationen$surgery.time.delta[i-1] <= 6 & H_Operationen$research_case_id[i-1] == H_Operationen$research_case_id[i])
  {
    # previous surgery and current surgery are the same
    H_Operationen$SurgeryNr[i] = H_Operationen$SurgeryNr[i-1]
  }
}

H_Operationen <- H_Operationen %>%
  # group additionally by new surgery NR
  group_by(research_case_id, SurgeryNr) %>%
  # earliest date first
  arrange(DtTmStop, .by_group = TRUE) %>%
  # take within grup teh earliest start date...
  mutate(Startdat.new = min(DtTmStart),
         #... and the latest stopdate
         Stopdat.new = max(DtTmStop), .after = Stopdat)

H_Operationen <- H_Operationen %>%
  # keep only first obs per group
  filter(row_number()==1) %>%
  ungroup() %>%
  mutate(Startdat = Startdat.new) %>%
  mutate(Stopdat = Stopdat.new) %>%
  dplyr::select(-c(surgery.time.delta, Startdat.new, Stopdat.new, DtTmStop, DtTmStart)) %>%
  relocate(SurgeryNr, .before = abgabedttm)


## I_Patient_Unfall
names(I_Patient_Unfall)[1] <- "Asa_vor_unfall"
I_Patient_Unfall <- I_Patient_Unfall[, -c(2, 3, 4, 5, 7:16, 18, 19, 25, 27, 29, 31:38)]
I_Patient_Unfall <- empty2NA(I_Patient_Unfall)

# Creating a variable for "Death"
I_Patient_Unfall$Death <- NA
for (i in 1:length(I_Patient_Unfall[, 1])) {
  if (is.na(I_Patient_Unfall[i, "Tod_Nach_Stunden"])) {
    I_Patient_Unfall[i, "Death"] <- 0
  }
  else {I_Patient_Unfall[i, "Death"] <- 1}
}

# Making gender a factor
I_Patient_Unfall[, 3] <- as.factor(I_Patient_Unfall[, 3])

# Categorizing age
x <- I_Patient_Unfall[, 4]
x[x=="79+"] <- 79
x <- as.numeric(x)
Age_Categ <-
  cut(
    x,
    breaks = c(0, seq(18, 78, by = 2), Inf),
    include.lowest = T,
    ordered_result = TRUE
  )
Age_Categ <- data.frame(Age_Categ)

I_Patient_Unfall <- cbind(I_Patient_Unfall, Age_Categ)
rm(Age_Categ)
I_Patient_Unfall <- subset(I_Patient_Unfall, select = c(1:4, 13, 5:12))
I_Patient_Unfall[, 4] <- x
names(I_Patient_Unfall)[4] <- "Age"

I_Patient_Unfall$Asa_vor_unfall <- as.factor(I_Patient_Unfall$Asa_vor_unfall)
I_Patient_Unfall$Tod_Eingetroffen_Datumzeit <- as.POSIXct(I_Patient_Unfall$Tod_Eingetroffen_Datumzeit, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich")


## J_PDMS_Mech_Beatmung
names(J_PDMS_Mech_Beatmung)[1] <- "MetaVision_Mechanische_Beatmung_Dauer_min"


## K_PDMS_Parameter
names(K_PDMS_Parameter)[1] <- "MetaVision_ParameterID"
# K_PDMS_Parameter$Time <- as.POSIXct(I_Patient_Unfall$Tod_Eingetroffen_Datumzeit, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich")
K_PDMS_Parameter$Time <- fastPOSIXct(K_PDMS_Parameter$Time)
K_PDMS_Parameter$Unitname <- gsub("^$|^ $", NA, K_PDMS_Parameter$Unitname)
K_PDMS_Parameter$Unitname <- gsub("Â°C", "C", K_PDMS_Parameter$Unitname)

## L_Rotem_Werte
names(L_Rotem_Werte)[1] <- "Dttm_of_analysis"
L_Rotem_Werte$Observation_value <- gsub("jpg", NA, L_Rotem_Werte$Observation_value)
L_Rotem_Werte$Reference_range <- gsub("-$", NA, L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("^$|^ $", NA, L_Rotem_Werte$Reference_range)

# The next few lines are not clear-cut (only partly successful at cleaning the data)
# The aim is to make sense out of the messy and inconsistent reference ranges
L_Rotem_Werte$Reference_range <- gsub("0.00-15.0", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("0.0-15", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("100-100", "100-200", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("100-200", "100-200", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("100-240", "100-200", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("100.0-240.0", "100-200", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("160-200", "160-200", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("160-211", "160-200", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("161-204", "160-200", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("161.0-204.0", "160-200", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("28-50", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("30-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("30-110", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("30-200", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("30-50", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("30-60", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("30.0-110", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("30.00-110.0", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("32-52", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("33-52", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("34-160", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("34-53", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("34-55", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("34.0-159", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("34.00-159.0", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("38-57", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("38-62", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("38-79", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("38.0-62.0", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("38.0-79.0", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("38.00-62.00", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("38.00-79.00", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("39-61", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("4-20", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("4.0-17", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("40-60", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("40-70", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("40-80", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("41-80", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("41.00-80.00", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("41.0-80.0", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("43-62", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("43-63", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("43-65", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("44-66", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("45-63", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("46-150", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("46-84", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("46.0-149", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("46.0-84.0", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("46.00-149.0", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("46.00-84.00", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("48-68", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("48-69", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("5-20", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("5.0-20", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("50-68", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("50-70", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("50-71", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("50-72", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("50-80", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("50.0-80.0", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("50.00-80.00", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("51-69", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("51-71", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("52-68", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("52-70", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("52-71", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("50-80", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("50.0-80.0", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("50.00-80.00", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("51-69", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("51-71", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("52-68", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("52-70", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("52-71", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("53-69", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("54-72", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("55-72", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("58-130", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("6-20", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("6.0-21", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("60-80", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("62-130", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("62-180", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("62.0-130", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("62.0-184", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("62.00-130.0", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("62.00-184.0", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("63-83", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("7-20", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("7.0-23", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("70-80", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("70-83", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("8-20", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("8.0-24", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("87-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("87.0-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("88-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("89-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("89.0-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("9-30", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("9.0-25", "0-20", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("90-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("91-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("91.0-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("92-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("92.0-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("93-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("93.0-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("94-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("94.0-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("98-100", "20-100", L_Rotem_Werte$Reference_range)
L_Rotem_Werte$Reference_range <- gsub("98.0-100", "20-100", L_Rotem_Werte$Reference_range)

# Make it ordered (still not an ideal solution as the intervals are overlapping)
L_Rotem_Werte$Reference_range <- factor(L_Rotem_Werte$Reference_range, ordered = T, levels = c("0-20", "20-100", "100-200", "160-200"))
L_Rotem_Werte$Dttm_of_analysis <- as.POSIXct(L_Rotem_Werte$Dttm_of_analysis, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich")


## M_Score_ISS_from_AIS
names(M_Score_ISS_from_AIS)[1] <- "ISS"


## N_Score_Saps
N_Score_Saps$DAT <- as.POSIXct(N_Score_Saps$DAT, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich")


## O_Score_Ips
O_Score_Ips[, c(7, 8)] <-
  lapply(O_Score_Ips[, c(7, 8)], function(i)
    as.POSIXct(i, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich"))
rm(O_Score_Ips) # should be excluded according to the Data Guide


## P_Stat_Teilstat_Faelle
P_Stat_Teilstat_Faelle[, 9] <- gsub("^$|^ $", NA, P_Stat_Teilstat_Faelle[, 9])
rm(P_Stat_Teilstat_Faelle) # should be excluded according to the Data Guide


## Q_Untersuchungen
Q_Untersuchungen$Observation_dttm <- as.POSIXct(Q_Untersuchungen$Observation_dttm, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich")

         
## R_Vital_Werte
R_Vital_Werte <- R_Vital_Werte[, -4]
R_Vital_Werte$STARTDAT <- as.POSIXct(R_Vital_Werte$STARTDAT, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Zurich")

# Creating the MAP variable
R_Vital_Werte$MAP <- (2 * R_Vital_Werte$Diastolic + R_Vital_Werte$Systolic) / 3

# Puls
R_Vital_Werte[which(R_Vital_Werte$Puls > 250), 4] <- NA # this
R_Vital_Werte[which(R_Vital_Werte$Puls < 15), 4] <- R_Vital_Werte[which(R_Vital_Werte$Puls < 15), 4] * 10

# Temperatur
R_Vital_Werte$Temperatur[which(R_Vital_Werte$Temperatur < 30)][1] <- 37.7
R_Vital_Werte$Temperatur[which(R_Vital_Werte$Temperatur < 30)][4] <- 37.3
R_Vital_Werte$Temperatur[which(R_Vital_Werte$Temperatur < 34)] <- NA


## Sascha's New File
S_DCO_Defsurg <- read.csv("S_DCO&Defsurg_Only_High_Interest_Cases.csv", sep = ";")
S_DCO_Defsurg <- S_DCO_Defsurg[!is.na(S_DCO_Defsurg$HighInterest), ]
S_DCO_Defsurg <- S_DCO_Defsurg[S_DCO_Defsurg$HighInterest == 1, ]
S_DCO_Defsurg <- S_DCO_Defsurg[, -c(3, 5)]


         

##### 2 ##### REMOVING ALL RESEARCH CASE IDS THAT DO NOT HAVE AN AIS>2 #####
source("helper_functions.R")

for (i in 1:length(ls())) {
  if (is.data.frame(get(ls()[i]))) {
    assign(ls()[i], filter_dataset(get(ls()[i])))
  }
}
         
## Adjusting Surgery Numbers
H_Operationen <-
  H_Operationen %>% group_by(research_case_id) %>% arrange(Startdat) %>% mutate(SurgeryNo = row_number())
H_Operationen <- subset(H_Operationen, select = c(26, 2:25))

