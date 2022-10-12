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
library(ggridges)
library(gridExtra)

base::load('data_icu_final.Rdata')

# start from here EDA

icu <- icu %>% rename(flag = y)
p1 <- icu %>%
  ggplot(aes(x = flag, fill = flag)) +
  geom_bar(alpha = 0.8) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = FALSE) +
  theme_bw()

p1

p2 <- icu %>%
  gather(x, y, c(Atemfrequenz_mean, FiO2_last, Herzfrequenz_max, Atemfrequenz_first, SpO2_mean, ArterialBP_first)) %>%
  ggplot(aes(x = y, y = flag, color = flag, fill = flag)) +
  facet_wrap( ~ x, scale = "free", ncol = 3) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer() +
  geom_density_ridges(alpha = 0.8) +
  guides(fill = FALSE, color = FALSE)

grid.arrange(p1, p2, ncol = 2, widths = c(0.3, 0.7))

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


ntrees <- c(300, 500, 1000)  
tune_grid <- expand.grid(.mtry = c(1: 4))

# training procedure
fitControl <- trainControl(
  # cross validation
  method = "cv",
  number = 5,
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  verbose = TRUE,
  classProbs = TRUE,
  savePredictions = TRUE
)

modellist <- list()

for (i in 1:length(ntrees)){
  ntree<-ntrees[i]
  set.seed(2049)
  rfs <- train(x = rec.syn, data = icu,
                  method = "rf", # Random Forest
                  trControl = fitControl,
                  tuneGrid = tune_grid,
                  # tuneLength  = 10, # Nr of random tuning parameters to try
                  metric = "ROC", # parameter values are chosen based on which metric (RMSE, MAE, R^2)
                  maximize = TRUE,
                  importance = TRUE,
                  ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- rfs
  
}

# plotting roc
roc_data <- roc(modellist$`300`$pred$obs, modellist$`300`$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
plot.roc(roc_data, main="ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA)

save(modellist, file = 'rfs.Rdata')
# variable importance

caret_imp <- varImp(modellist$`1000`)
caret_imp
plot(caret_imp, top=10)

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
