# Set directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages
library(tidyverse)
library(recipes)
library(caret)
library(caretEnsemble)
library(doParallel)
library(timeDate)

# set up clusters for parallelization
numCores <- detectCores() - 1

# load ICU data with labels
load("./../Data/Modelling/W_ICU_Features.Rdata")
train <- train[complete.cases(as.matrix(train)),] %>%
  dplyr::select(-c(research_case_id, DtTmStart, DtTmStop)) %>%
  mutate(SurgeryNr = factor(SurgeryNr))

# Pre-Process Recipe 1

# list of available recipe steps:
# https://www.tidymodels.org/find/recipes/

# Selectors:
# https://recipes.tidymodels.org/reference/selections.html

# find variables with missings
missing.vars <- colnames(data.syn)[sapply(data.syn, function(x) any(is.na(x)))]

# define the pre-processing steps
rec <- recipe(y~., data = train) %>%
  # center and scale
  step_normalize(all_numeric_predictors()) %>%
  # create dummies
  step_dummy(all_nominal_predictors()) %>%
  # remove nzv
  step_nzv(all_predictors()) %>%
  # remove linearly dependent variables
  step_lincomb(all_predictors())

# for assessing the outcome
rec_prep <- prep(rec, training = train, retain = TRUE, verbose = TRUE)
design.mat <- bake(rec_prep, new_data = train)

# Identify most Promising Models without Tuning

# training procedure
fitControl <- trainControl(
  # cross validation
  method = "cv",
  number = 5,
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  verbose = TRUE,
  classProbs = TRUE,
)

set.seed(101)
# start parallel
my.cluster <- makeForkCluster(numCores, setup_strategy = "sequential")
registerDoParallel(my.cluster)
fit <- train(rec, data = train,
             method = "ranger",
             trControl = fitControl,
             tuneLength  = 1, # Nr of random tuning parameters to try
             metric = "ROC", # parameter values are chosen based on whcih metric (RMSE, MAE, R^2)
             maximize = TRUE
)
stopCluster(my.cluster)
fit$results

# Fit Most Promising Models

models <- c("ORFridge", "xgbDART", "glmnet", "gaussprPoly", "pcaNNet", "ranger", "gbm", "svmRadialSigma")

# training procedure
fitControl <- trainControl(
  # cross validation
  method = "repeatedcv",
  number = 5, repeats = 5,
  index=createResample(train$y, 5),
  savePredictions="final",
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  verbose = TRUE,
  classProbs = TRUE,
)

set.seed(101)
# start parallel
my.cluster <- makeForkCluster(numCores, setup_strategy = "sequential")
registerDoParallel(my.cluster)
fit1 <- caretList(rec, data = train,
             methodList= models,
             trControl = fitControl,
             tuneLength  = 1, # Nr of random tuning parameters to try
             metric = "ROC", # parameter values are chosen based on whcih metric (RMSE, MAE, R^2)
             maximize = TRUE
)
stopCluster(my.cluster)
save(fit1, file = "fit1.Rdata")

# get the Mean of the ROC over the resample
mean(summary(resamples(fit1))$statistics$ROC[,4])

# Pre-Process Recipe 2 (No Model Pre-selection)

# define the pre-processing steps
rec <- recipe(y~., data = train) %>%
  # center and scale
  step_normalize(all_numeric_predictors()) %>%
  # create dummies
  step_dummy(all_nominal_predictors())

# for assessing the outcome
rec_prep <- prep(rec, training = train, retain = TRUE, verbose = TRUE)
design.mat <- bake(rec_prep, new_data = train)

train.num <- train %>% dplyr::select(where(is.numeric))
skew.vars <- apply(train.num , 2, skewness, na.rm = TRUE)
skew.vars <- skew.vars[order(names(skew.vars))]

skew.vars.after <- apply(design.mat[,names(train.num)], 2, skewness, na.rm = TRUE)
skew.vars.after <- skew.vars.after[order(names(skew.vars.after))]
cbind(skew.vars, skew.vars.after)

# Fit

models <- c("ORFridge", "xgbDART", "glmnet", "gaussprPoly", "pcaNNet", "ranger", "gbm", "svmRadialSigma")

set.seed(101)
# start parallel
my.cluster <- makeForkCluster(numCores, setup_strategy = "sequential")
registerDoParallel(my.cluster)
fit2 <- caretList(rec, data = train,
                  methodList= models,
                  trControl = fitControl,
                  tuneLength  = 1, # Nr of random tuning parameters to try
                  metric = "ROC", # parameter values are chosen based on whcih metric (RMSE, MAE, R^2)
                  maximize = TRUE
)
stopCluster(my.cluster)
save(fit2, file = "fit2.Rdata")

# get the Mean of the ROC over the resample
mean(summary(resamples(fit2))$statistics$ROC[,4])

# Pre-Process Recipe 3 (Remove Highly Corr)

# define the pre-processing steps
rec <- recipe(y~., data = train) %>%
  # center and scale
  step_normalize(all_numeric_predictors()) %>%
  # create dummies
  step_dummy(all_nominal_predictors()) %>%
  # remove nzv
  step_nzv(all_predictors()) %>%
  # remove linearly dependent variables
  step_lincomb(all_predictors()) %>%
  step_corr(all_predictors(), threshold = .95)

# for assessing the outcome
rec_prep <- prep(rec, training = train, retain = TRUE, verbose = TRUE)
design.mat <- bake(rec_prep, new_data = train)

train.num <- train %>% dplyr::select(where(is.numeric))
skew.vars <- apply(train.num , 2, skewness, na.rm = TRUE)
skew.vars <- skew.vars[order(names(skew.vars))]

skew.vars.after <- apply(design.mat[,names(train.num)], 2, skewness, na.rm = TRUE)
skew.vars.after <- skew.vars.after[order(names(skew.vars.after))]
cbind(skew.vars, skew.vars.after)

# Fit (Worse!)

models <- c("ORFridge", "xgbDART", "glmnet", "gaussprPoly", "pcaNNet", "ranger", "gbm", "svmRadialSigma")

set.seed(101)
# start parallel
my.cluster <- makeForkCluster(numCores, setup_strategy = "sequential")
registerDoParallel(my.cluster)
fit3 <- caretList(rec, data = train,
                  methodList= models,
                  trControl = fitControl,
                  tuneLength  = 1, # Nr of random tuning parameters to try
                  metric = "ROC", # parameter values are chosen based on whcih metric (RMSE, MAE, R^2)
                  maximize = TRUE
)
stopCluster(my.cluster)
save(fit3, file = "fit3.Rdata")

# get the Mean of the ROC over the resample
mean(summary(resamples(fit3))$statistics$ROC[,4])

# Pre-Process Recipe 4 (PCA)
  
# define the pre-processing steps
rec <- recipe(y~., data = train) %>%
  # center and scale
  step_normalize(all_numeric_predictors()) %>%
  # create dummies
  step_dummy(all_nominal_predictors()) %>%
  # PCA
  step_pca(all_predictors(), threshold = .95) %>%
  # remove nzv
  step_nzv(all_predictors()) %>%
  # remove linearly dependent variables
  step_lincomb(all_predictors())

# Fit (Worse!)

models <- c("ORFridge", "xgbDART", "glmnet", "gaussprPoly", "pcaNNet", "ranger", "gbm", "svmRadialSigma")

set.seed(101)
# start parallel
my.cluster <- makeForkCluster(numCores, setup_strategy = "sequential")
registerDoParallel(my.cluster)
fit4 <- caretList(rec, data = train,
                  methodList= models,
                  trControl = fitControl,
                  tuneLength  = 1, # Nr of random tuning parameters to try
                  metric = "ROC", # parameter values are chosen based on whcih metric (RMSE, MAE, R^2)
                  maximize = TRUE
)
stopCluster(my.cluster)
save(fit4, file = "fit4.Rdata")

# get the Mean of the ROC over the resample
mean(summary(resamples(fit4))$statistics$ROC[,4])

# Pre-Process Recipe 4 (YeoJohnson)

rec <- recipe(y~., data = train) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  # center and scale
  step_normalize(all_numeric_predictors()) %>%
  # create dummies
  step_dummy(all_nominal_predictors()) %>%
  # remove nzv
  step_nzv(all_predictors()) %>%
  # remove linearly dependent variables
  step_lincomb(all_predictors())

# Fit (Worse!)

models <- c("ORFridge", "xgbDART", "glmnet", "gaussprPoly", "pcaNNet", "ranger", "gbm", "svmRadialSigma")

set.seed(101)
# start parallel
my.cluster <- makeForkCluster(numCores, setup_strategy = "sequential")
registerDoParallel(my.cluster)
fit5 <- caretList(rec, data = train,
                  methodList= models,
                  trControl = fitControl,
                  tuneLength  = 1, # Nr of random tuning parameters to try
                  metric = "ROC", # parameter values are chosen based on whcih metric (RMSE, MAE, R^2)
                  maximize = TRUE
)
stopCluster(my.cluster)
save(fit5, file = "fit5.Rdata")

# get the Mean of the ROC over the resample
mean(summary(resamples(fit5))$statistics$ROC[,4])

# Compare Fits

fits <- cbind(summary(resamples(fit1))$statistics$ROC[,4],
              summary(resamples(fit2))$statistics$ROC[,4],
              summary(resamples(fit3))$statistics$ROC[,4],
              summary(resamples(fit4))$statistics$ROC[,4],
              summary(resamples(fit5))$statistics$ROC[,4])
colnames(fits) <- c("reduced", "full", "corr", "pca", "yeo")
colMeans(fits)

modelCor(resamples(fit1))

# Final Recipe

# define the pre-processing steps
rec <- recipe(y~., data = train) %>%
  # center and scale
  step_normalize(all_numeric_predictors()) %>%
  # create dummies
  step_dummy(all_nominal_predictors()) %>%
  # remove nzv
  step_nzv(all_predictors()) %>%
  # remove linearly dependent variables
  step_lincomb(all_predictors())

# Final Fit

models <- c("ORFridge", "xgbDART", "glmnet", "gaussprPoly", "pcaNNet", "ranger", "gbm", "svmRadialSigma")

# training procedure
fitControl <- trainControl(
  # cross validation
  method = "repeatedcv",
  number = 10, repeats = 5,
  index=createResample(train$y, 5),
  search = "random",
  savePredictions="final",
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  verbose = TRUE,
  classProbs = TRUE,
)

set.seed(101)
# start parallel
my.cluster <- makeForkCluster(numCores, setup_strategy = "sequential")
registerDoParallel(my.cluster)
fit_final <- caretList(rec, data = train,
                  methodList= models,
                  trControl = fitControl,
                  tuneLength  = 100, # Nr of random tuning parameters to try
                  metric = "ROC", # parameter values are chosen based on whcih metric (RMSE, MAE, R^2)
                  maximize = TRUE
)
stopCluster(my.cluster)
save(fit_final, file = "fit_final.Rdata")

# get the Mean of the ROC over the resample
mean(summary(resamples(fit_final))$statistics$ROC[,4])

# Chose Models for Ensemble

# check correlation among models
modelCor(resamples(fit_final))
# chose ORFridge, gaussprPoly, pcaNNet, svmRadialSigma

# training procedure
fitControl <- trainControl(
  # cross validation
  method = "repeatedcv",
  number = 10, repeats = 5,
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  verbose = TRUE,
  savePredictions="final",
  classProbs = TRUE,
)

set.seed(101)
# start parallel
my.cluster <- makeForkCluster(numCores, setup_strategy = "sequential")
registerDoParallel(my.cluster)
greedy_ensemble <- caretEnsemble(
  fit_final[c("ORFridge", "xgbDART", "pcaNNet")], 
  metric="ROC",
  trControl= fitControl)
stopCluster(my.cluster)

set.seed(101)
# start parallel
my.cluster <- makeForkCluster(numCores, setup_strategy = "sequential")
registerDoParallel(my.cluster)
glm_ensemble <- caretStack(
  fit_final[c("ranger", "glmnet", "pcaNNet")], 
  method="glmnet",
  metric="ROC",
  search = "random",
  maximize = TRUE,
  tuneLength  = 100,
  trControl= fitControl
)
stopCluster(my.cluster)

glm_ensemble$ens_model$results
