library(tidyverse)
library(recipes)
library(caret)
library(doParallel)
library(xgboost)
library(tidymodels)
library(plotROC)
library(MLeval)
library(pROC)
library(DiagrammeR)
library(glmnet)
library(finalfit)
library(corrplot)
library(butcher)
library(dplyr)
setwd("~/Desktop/Data Folder - StatsLab - May 21")

# set up clusters for parallelization
numCores <- detectCores() - 1


# load fully imputed data -----
load("unimputed_complete_cases.Rdata")
data.syn <- df_complete


count_variables_vit <- names(data.syn)[grep('_n', names(data.syn))][1:4]
count_variables_bga <- names(data.syn)[grep('_n', names(data.syn))][4:6]
count_variables_blut <- names(data.syn)[grep('_n', names(data.syn))][7:19]

data.syn$n_vit <- apply(data.syn[ , count_variables_vit], 1, median)
data.syn$n_bga <- apply(data.syn[ , count_variables_bga], 1, median)
data.syn$n_blut <- apply(data.syn[ , count_variables_blut], 1, median)

# remove # of observations for each measurement

data.syn <- data.syn %>% dplyr::select(-c(count_variables_bga, count_variables_blut, count_variables_vit))

# removing the median as we already have the mean and PCTB
data.syn <- data.syn %>% dplyr::select(-matches("median"))%>% 
  dplyr::select(-matches("PCTB"))
# really only taking the mean and sd for now
data.syn <- data.syn %>% dplyr::select(-matches("min|max|skew"))
# keeping age and gender and converting to factors


data.syn <- data.syn %>% dplyr::select(-c(Asa_vor_unfall, Primaer_oder_zuweisung, Trauma_mechanismus, Schwangerschaft,  Age_Categ))


factor_var_01 <- c('flag', 'icu', "PTZEIT","HUFH","PCT",
                   "LYM", "NEU", "LDH","GGT" ,
                   "IMGR","IMGRA" ,"MYO","TZI")

data.syn <- data.syn %>%
  mutate(across(factor_var_01, 
                ~factor(ifelse(.x == "1","Yes","No"))))

data.syn[, c('Geschlecht', 'head_injury')] <- lapply(data.syn[,  c('Geschlecht', 'head_injury')],
                                                   factor)

numeric_vars <- select_if(data.syn, is.numeric)


data.syn <- data.syn[, -1]

tune_grid <- expand.grid(
  alpha = c(0, 0.3, 0.5, 0.7, 1), # ridge, elastic-net, lasso in that order
  lambda = seq(0.001, 1, length = 20)
)

rec.syn <- recipe(flag ~ ., data = data.syn) %>%
  # impute data
  # step_impute_mean(all_of(missing.vars)) %>%
  # center and scale
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  # # create dummies
  step_dummy(all_nominal_predictors()) %>%
  # remove nzv
  #step_nzv(all_predictors()) %>%
  # remove linearly dependent variables
  step_lincomb(all_predictors())

# check preprocessing
# rec_prep <- prep(rec.syn, training = data.syn, retain = TRUE, verbose = TRUE)
# design.mat <- bake(rec_prep, new_data = data.syn)


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
)

cl <- makeForkCluster(numCores, setup_strategy = "sequential")
registerDoParallel(cl)

set.seed(2049)

penalised_lgr <- caret::train(x = rec.syn, data = data.syn,
                              method = "glmnet",
                              family = "binomial",
                              trControl = fitControl,
                              tuneGrid = tune_grid,
                              metric = "ROC",
                              maximize = TRUE)


stopCluster(cl)


plot(penalised_lgr)
penalised_lgr$bestTune

coef(penalised_lgr$finalModel, penalised_lgr$finalModel$lambdaOpt)

### plotting roc
test <- roc(penalised_lgr$pred$obs, penalised_lgr$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
plot.roc(test, main="ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA)


# random forest --------
fitControl <- trainControl(
  # cross validation
  method = "cv",
  number = 10,
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  verbose = TRUE,
  classProbs = TRUE,
  savePredictions = TRUE
)
cl <- makeForkCluster(numCores, setup_strategy = "sequential")

registerDoParallel(cl)

set.seed(2049)

fit_rf <- caret::train(rec.syn, data = data.syn,
             method = "rf",
             trControl = fitControl,
             tuneLength  = 10, # Nr of random tuning parameters to try
             metric = "ROC", # parameter values are chosen based on whcih metric (RMSE, MAE, R^2)
             maximize = TRUE
)


stopCluster(cl)

test <- roc(fit_rf$pred$obs, fit_rf$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
plot.roc(test, main="ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA)
varImp(fit_rf, scale = FALSE)


# xgboost -------

fitControl <- trainControl(
  # cross validation
  method = "cv",
  number = 10,
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  verbose = TRUE,
  classProbs = TRUE,
  savePredictions = TRUE
)
cl <- makeForkCluster(numCores, setup_strategy = "sequential")

registerDoParallel(cl)

set.seed(2049)

fit_xg <- caret::train(rec.syn, data = data.syn,
                       method = "xgbTree",
                       trControl = fitControl,
                       tuneLength  = 10, # Nr of random tuning parameters to try
                       metric = "ROC", # parameter values are chosen based on whcih metric (RMSE, MAE, R^2)
                       maximize = TRUE
)


stopCluster(cl)

test <- roc(fit_xg$pred$obs, fit_xg$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
plot.roc(test, main="ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA)
varImp(fit_xg, scale = FALSE)

# repeat for imputed
save(penalised_lgr, file = 'unimputed_penalised_regression.RData')     
save(fit_rf, file = 'unimputed_rf.RData')     
save(fit_xg, file = 'unimputed_xg.RData')     

