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

base::load('data_icu_final.Rdata')

icu[complete.cases(icu),] %>% dim

missing.vars <- colnames(icu)[sapply(icu, function(x) any(is.na(x)))]

rec.syn <- recipe(y ~ ., data = icu) %>%
  # impute data
  step_impute_median(all_of(missing.vars)) %>%
  # center and scale
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  # create dummies
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

xgb_tune$bestTune

xgb_tune$results

xgb_tune$pred

# plotting roc
roc_data <- roc(xgb_tune$pred$obs, xgb_tune$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
plot.roc(roc_data, main="ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA)

# variable importance
caret_imp <- varImp(xgb_tune)
caret_imp
plot(caret_imp, top=10)

xgb_imp <- xgb.importance(feature_names = xgb_tune$finalModel$feature_names,
                          model = xgb_tune$finalModel)
xgb.ggplot.importance(xgb_imp, top_n = 10)
xgb.imp

save(xgb_tune, file = 'taru/adn_xgb_model.Rdata')

result <- evalm(xgb_tune, title = "5-fold CV ROC", gnames = c('xgboost'))
result$roc

test <- pROC::roc(xgb_tune$pred$obs, xgb_tune$pred$Yes, levels = c("No", "Yes"))
plot.roc(test, main="ROC", col = "red", print.auc = TRUE)
save(xgb_tune, file ='xgb_model.Rdata')

# looking at complete cases only
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
