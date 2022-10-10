library(tidyverse)
library(recipes)
library(caret)
library(visdat)
library(xgboost)
library(tidymodels)
library(plotROC)
library(pROC)
library(glmnet)
library(finalfit)
library(corrplot)
library(gridExtra)

base::load('icu.Rdata')

################################## dig more into the data and do a bit more cleaning

# removing some exist variables - such as temperature, height, weight

icu <- icu %>% select(-c(Temperatur_exist, Groesse_exist, Gewicht_exist))

## rather than keeping all the n's for each measurement, compute a median to have an idea of # of measurements collected

count_variables <- icu %>% select(matches("_n")) %>% names()

icu$median_no_measurments <- apply(icu[ , count_variables], 1, median)

# remove # of observations for each measurement

icu <- icu %>% select(-matches("_n"))

# removing the median as we already have the mean
icu <- icu %>% select(-matches("median"))

# keeping age and gender

icu <- icu %>% select(-c(Asa_vor_unfall, Primaer_oder_zuweisung, Trauma_mechanismus, Schwangerschaft))

numeric_vars <- select_if(icu, is.numeric)

cor_matrix <- cor(numeric_vars, use = 'complete.obs')

pdf(file = "corr_matrix.pdf", width = 10, height = 9)
corrplot(cor_matrix, method="circle", type = 'upper', tl.cex=0.7) # I think one of the 2 kurtosis or skweness needs removing
dev.off()



icu <- icu %>% select(-matches("kurt"))

vis_miss(icu, sort_miss = TRUE) + 
  theme(axis.text.x = element_text(angle = 90))

save(icu, file='cleaned_icu_data.Rdata')

write.csv(icu, 'icu_data.csv', row.names = F)

# icu_try <- icu %>% select(-matches("Atemfrequenz"))

icu <- icu %>% select(-SurgeryNr)

save(icu, file = 'data_icu_final.Rdata')

#######################################################################################################
########################################################################## complete cases variant
# complete <- train[complete.cases(train),]
# 
# #### standardise the data - biased
# 
# numeric_variables <- complete %>% select(-c(list_factors, y))
# trainMean <- apply(numeric_variables,2,mean)
# trainSd <- apply(numeric_variables,2,sd)
# 
# norm_data <- sweep(sweep(numeric_variables, 2L, trainMean), 2, trainSd, "/")
# y <- complete$y
# train <- cbind(y, norm_data, complete %>% select(-y) %>% select(list_factors))

##################################################################################

###########################################################################
### Loading cleaned data for training

# load('cleaned_icu_data.Rdata')

base::load('data_icu_final.Rdata')

missing.vars <- colnames(icu)[sapply(icu, function(x) any(is.na(x)))]

tune_grid <- expand.grid(
  alpha = c(0, 0.3, 0.5, 0.7, 1), # ridge, elastic-net, lasso in that order
  lambda = seq(0.001, 1, length = 20)
)

colnames(icu)[4:44]

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

# check preprocessing
rec_prep <- prep(rec.syn, training = icu, retain = TRUE, verbose = TRUE)
design.mat <- bake(rec_prep, new_data = icu)


# training procedure
fitControl <- trainControl(
  # cross validation
  method = "cv",
  number = 10,
  # allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  verbose = TRUE,
  classProbs = TRUE,
  savePredictions = TRUE
)

set.seed(2049)

penalised_lgr <- train(x = rec.syn, data = icu,
                  method = "glmnet",
                  family = "binomial",
                  trControl = fitControl,
                  tuneGrid = tune_grid,
                  metric = "ROC",
                  maximize = TRUE)

plot(penalised_lgr)
penalised_lgr$bestTune

coefs_penalised_log_reg <- as.data.frame(as.matrix(coef(penalised_lgr$finalModel, penalised_lgr$finalModel$lambdaOpt)))

coefs_penalised_log_reg <- coefs_penalised_log_reg %>% rename(Beta = 1) %>% filter(Beta!= 0.0)

## saving results to image

myTable <- tableGrob(
  coefs_penalised_log_reg, 
  # rows = NULL, 
  theme = ttheme_default(core = list(bg_params = list(fill = "grey99")))
)
grid::grid.draw(myTable)

penalised_lgr$finalModel$beta

### plotting roc
test <- roc(penalised_lgr$pred$obs, penalised_lgr$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
plot.roc(test, main="ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA)

lgr <- penalised_lgr
save(lgr, file = 'taru/adn_penalised_lgr.Rdata')


###########################################################
#### running without caret

rec_prep <- prep(rec.syn, training = icu, retain = TRUE, verbose = TRUE)
design.mat <- bake(rec_prep, new_data = icu)

design.mat <- data.matrix(design.mat)

model_lgr <- cv.glmnet(x = design.mat[ ,!(colnames(design.mat) == "y")], y = design.mat[, 'y'],
                    family="binomial", trace.it = TRUE, nfolds=5, type.measure = "auc")
coef(model_lgr)
plot(model_lgr)

lambda_opt <- model_lgr$lambda.min 

refit_glmnet <- glmnet(x = design.mat[ ,!(colnames(design.mat) == "y")], y = design.mat[, 'y'],
                       family="binomial", lambda = lambda_opt)

coef(refit_glmnet)

##### converting back to dataframe

rec_prep <- prep(rec.syn, training = icu, retain = TRUE, verbose = TRUE)
design.df <- bake(rec_prep, new_data = icu)

glm_fit <- glm(formula = y ~ . - y, 
    family = binomial, data = icu)

summary(glm_fit)
