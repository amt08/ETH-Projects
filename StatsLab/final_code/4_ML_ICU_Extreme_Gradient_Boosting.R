library(tidyverse)
library(recipes)
library(caret)
library(doParallel)
library('visdat')
library("xgboost")
library(tidymodels)
library(plotROC)
library(MLeval)
library(pROC)
library(DiagrammeR)

base::load('icu.Rdata')

#################
####################################### ignore commented part but still useful to see what has been done before

# icu <- icu %>% dplyr::select(-SurgeryNr)

# icu_complete <- icu[complete.cases(icu),]

# base::load('data_icu_final.Rdata')
# base::load('W_ICU_Features.Rdata')
# base::load('data/tidy_data/model/W_ICU_Features.Rdata')

# vis_miss(icu_data)
# single_measurements <- read.csv('data/tidy_data/model/U_Single_Measurements_Patient.csv', stringsAsFactors = T)
# 
# no_complications <- single_measurements %>% select(-starts_with("complication"))
# 
# # vis_miss(no_complications)
# 
# # deleting death after hours variable
# 
# no_complications <- no_complications %>% select(-c(death_after_hours, Death, research_id, mechanical_breathing_mins, duration_hospital_stay_days)) %>% distinct()
# outcome.vitals <- icu_data
# outcome.vitals <- outcome.vitals %>% select(-c(DtTmStart, DtTmStop))
# 
# # merge the data with ICU data
# 
# data <- outcome.vitals %>% left_join(no_complications, by = 'research_case_id') %>% select(-research_case_id)
# 
# ## make everything a number as xgbtree works with numbers only
# 
# to_numeric <- function(data){
#   
#   data$Groesse_exist <- as.numeric(data$Groesse_exist)
#   # levels(data$Groesse_exist) <- c("No","Yes")
#   data$Gewicht_exist <- as.numeric(data$Gewicht_exist)
#   # levels(data$Gewicht_exist) <- c("No","Yes")
#   data$Temperatur_exist <- as.numeric(data$Temperatur_exist)
#   # levels(data$Temperatur_exist) <- c("No","Yes")
#   data$Verbale_exist <- as.numeric(data$Verbale_exist)
#   # levels(data$Verbale_exist) <- c("No","Yes")
#   data$Motorische_exist <- as.numeric(data$Motorische_exist)
#   # levels(data$Motorische_exist) <- c("No","Yes")
#   data$Augen_exist <- as.numeric(data$Augen_exist)
#   # levels(data$Augen_exist) <- c("No","Yes")
#   
#   # only y needs to be left as factor
#   data$y <- as.factor(data$y)
#   levels(data$y) <- c("No", "Yes")
#   
#   return(data)
#   
# }
# 
# data <- data %>%
#   rename(y = flag)
# 
# train <- to_numeric(data)

# metrics <- metric_set(roc_auc, accuracy)

############################################################### start from here

base::load('data_icu_final.Rdata')

icu[complete.cases(icu),] %>% dim

missing.vars <- colnames(icu)[sapply(icu, function(x) any(is.na(x)))]

rec.syn <- recipe(y ~ ., data = icu) %>%
  # impute data
  step_impute_median(all_of(missing.vars)) %>%
  # center and scale
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  # # create dummies
  step_dummy(all_nominal_predictors()) %>%
  # remove nzv
  step_nzv(all_predictors()) %>%
  # remove linearly dependent variables
  step_lincomb(all_predictors())

tune_grid <- expand.grid(
  max_depth = c(3, 5),
  nrounds = seq(10, 1000, by = 100),
  eta = c(0.01,0.05, 0.1, 0.15),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# training procedure
fitControl <- trainControl(
  # cross validation
  method = "cv",
  number = 10,
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  verbose = TRUE,
  classProbs = TRUE,
  savePredictions = TRUE
  # seeds =  cvSeeds
)

set.seed(2049)

xgb_tune <- train(x = rec.syn, data = icu,
                  method = "xgbTree",
                  trControl = fitControl,
                  tuneGrid = tune_grid,
                  # tuneLength  = 10, # Nr of random tuning parameters to try
                  metric = "ROC", # parameter values are chosen based on which metric (RMSE, MAE, R^2)
                  maximize = TRUE
)

# turn parallel processing off and run sequentially again:
# registerDoSEQ()

xgb_tune$bestTune

xgb_tune$results

xgb_tune$pred

### plotting roc
roc_data <- roc(xgb_tune$pred$obs, xgb_tune$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
plot.roc(roc_data, main="ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA)


############################ variable importance

caret_imp <- varImp(xgb_tune)
caret_imp
plot(caret_imp, top=10)

# ggplot(caret_imp) +
#   theme_minimal()

xgb_imp <- xgb.importance(feature_names = xgb_tune$finalModel$feature_names,
                          model = xgb_tune$finalModel)

xgb.ggplot.importance(xgb_imp, top_n = 10)

xgb.imp

save(xgb_tune, file = 'taru/adn_xgb_model.Rdata')
##### plotting ROC curves

# pred_y <- xgb_tune$pred$pred
# levels(pred_y) <- c(0, 1)
# pred_y <- as.numeric(pred_y)
# 
# true_y <- xgb_tune$pred$obs
# levels(true_y) <- c(0, 1)
# true_y <- as.numeric(true_y)
# 
# plot(roc(predictor = pred_y, response = true_y, auc=TRUE), main = "ROC")
# 
# 
# xgb_tune$pred
# 
# xgb_tune$finalModel$obsLevels

result <- evalm(xgb_tune, title = "5-fold CV ROC", gnames = c('xgboost'))

result$roc

test <- pROC::roc(xgb_tune$pred$obs, xgb_tune$pred$Yes, levels = c("No", "Yes"))
plot.roc(test, main="ROC", col = "red", print.auc = TRUE)

# head(xgb_tune$pred)
# 
# xgb.plot.tree(model = xgb_tune$finalModel, trees = 1, plot_width = 1000, plot_height = 900)
# 
# xgb.plot.multi.trees(model = xgb_tune$finalModel)

save(xgb_tune, file ='xgb_model.Rdata')
####################################################################
###################### looking at complete cases only

all_missing <- names(which(colSums(is.na(train)) > 0))

complete_cases_icu <- train[complete.cases(train),]

set.seed(2049)

xgb_tune <- train(x = subset(complete_cases_icu, select = -y),
                  y = complete_cases_icu$y,
                  method = "xgbTree",
                  trControl = fitControl,
                  tuneGrid = tune_grid,
                  # tuneLength  = 10, # Nr of random tuning parameters to try
                  metric = "ROC", # parameter values are chosen based on which metric (RMSE, MAE, R^2)
                  maximize = TRUE
)


result <- evalm(xgb_tune, title = "5-fold CV ROC", gnames = c('xgboost'))

result$roc

