library(visdat)
library(dplyr)
setwd("~/Desktop/Data Folder - StatsLab - May 21")


load('unimputed_all_cases.RData')

missing_percentage <- sapply(df, function(x) {sum(is.na(x))/nrow(df)})

nm <- names(df)[grep('_mean', names(df))]
vis_dat(df[, nm])
