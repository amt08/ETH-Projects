rm(list = ls(all.names = TRUE))
library(dplyr)
library(tidyverse)
library(MASS)
library(caret)
library(glmnet)
setwd("~/Desktop/Data Folder - StatsLab - May 21")

# data preparation -------------
load('unimputed_complete_cases.RData') # without imputation
design.mat <- df_complete

model <- glm(flag~., data = design.mat[, -1], family = binomial, 
             control = list(maxit = 50)) %>% stepAIC(direction = 'forward') #AIC: 490

summary(model)
x <- design.mat[, -c(1, 3)]
y <- design.mat[, 'flag']
glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)

# a very small model: just vitals --------
design_subset <- design.mat[, c(1:43)]

model2 <- glm(flag~., data = design_subset[, -1], family = binomial, 
    control = list(maxit = 50)) %>% stepAIC(direction = 'forward') # AIC: 491.9


corrplot::corrplot(cor(design_subset[, -1]))

# glmm: just to try -----
library("lme4")


glmer(flag~SurgeryNr+Temperatur_mean+Puls_median+Diastolic_min+Systolic_max+(1|research_case_id),
      data = design_subset, family = 'binomial', 
      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

