
# Prepare the environment and load Data----

# Set directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages
library(tidyverse)
library(recipes)
library(caret)
library(doParallel)

# set up clusters for parallelization
numCores <- detectCores() - 1
#my.cluster <- makePSOCKcluster(numCores) #makeForkCluster(numCores)

# load synthesized data
load("./usedata/AB4x_train.Rdata")
data.syn <- data
rm(data)

# Recipe for Pre-Processing----

# list of available recipe steps:
# https://www.tidymodels.org/find/recipes/

# Selectors:
# https://recipes.tidymodels.org/reference/selections.html

# 1) Pre-Process Recipe synthetic data

# find variables with missings
missing.vars <- colnames(data.syn)[sapply(data.syn, function(x) any(is.na(x)))]

# define the pre-processing steps
rec.syn <- recipe(emig~., data = data.syn) %>%
  # impute with bagged trees
  step_impute_bag(all_of(missing.vars)) %>%
  # center and scale
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  # create dummies
  step_dummy(all_nominal_predictors()) %>%
  # remove nzv
  step_nzv(all_predictors()) %>%
  # remove linearly dependent variables
  step_lincomb(all_predictors())

rec_prep <- prep(rec.syn, training = data.syn, retain = TRUE, verbose = TRUE)
design.mat <- bake(rec_prep, new_data = data.syn)

# Model Fit----

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
fit <- train(rec, data = data,
             method = "rf",
             trControl = fitControl,
             tuneLength  = 10, # Nr of random tuning parameters to try
             metric = "ROC", # parameter values are chosen based on whcih metric (RMSE, MAE, R^2)
             maximize = TRUE
)
stopCluster(my.cluster)
  

# save syn fit for evaluation testing:
save(fit.syn, file = "./usedata/Testing/fit.RData")

# Test Set Prediction (if applicable)----

pred.prob <- predict(fit, newdata = test.x, type = "prob")
pred.level <- predict(fit, newdata = test.x)

pred <- cbind(pred = pred.level, obs = test.y,
              pred.prob)

glm.sum1 <- twoClassSummary(pred, lev = levels(test.y))
glm.sum2 <- confusionMatrix(data = pred.level, reference = test.y)
