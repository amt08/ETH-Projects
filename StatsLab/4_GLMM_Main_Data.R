library(dplyr)
library(lme4)
library(plotROC)
library(pROC)

cross_val <- function(x, k = 10, formula) {
  pred <- numeric(nrow(x))
  
  ind <- sample(1:k, nrow(x), replace = TRUE)
  
  for (i in 1:k) {
    ind_i <- ind==i
    train <- x[!ind_i, ]
    test <- x[ind_i, ]
    
    mod <- glmer(formula = formula,
                 data = train, family = 'binomial',
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
    
    pred[ind_i] <- predict(mod, newdata = test, allow.new.levels = TRUE)
  }
  pred
}

# load fully imputed data with missing values
load("df_all_imputed.Rdata")
data.syn <- df # with missing data

# count variables for the three types of data. Very highly correlated
count_variables_vit <- names(data.syn)[grep('_n', names(data.syn))][1:4]
count_variables_bga <- names(data.syn)[grep('_n', names(data.syn))][4:6]
count_variables_blut <- names(data.syn)[grep('_n', names(data.syn))][7:19]

data.syn$n_vit <- apply(data.syn[ , count_variables_vit], 1, median)
data.syn$n_bga <- apply(data.syn[ , count_variables_bga], 1, median)
data.syn$n_blut <- apply(data.syn[ , count_variables_blut], 1, median)

# remove # of observations for each measurement
data.syn <- data.syn %>% 
  dplyr::select(-c(count_variables_bga, count_variables_blut, count_variables_vit))

# removing the median as we already have the mean and PCTB
data.syn <- data.syn %>% dplyr::select(-matches("median"))%>% 
  dplyr::select(-matches("PCTB"))

# keeping age and gender and converting to factors
data.syn <- data.syn %>% 
  dplyr::select(-c(Asa_vor_unfall, Primaer_oder_zuweisung, Trauma_mechanismus, Schwangerschaft))

# converting variables into factors
factor_var_01 <- c('flag', 'icu', "PTZEIT","HUFH","PCT",
                   "LYM", "NEU", "LDH","GGT" ,
                   "IMGR","IMGRA" ,"MYO","TZI")

data.syn <- data.syn %>%
  mutate(across(factor_var_01, 
                ~factor(ifelse(.x == "1","Yes","No"))))

data.syn[, c('Geschlecht', 'Age_Categ', 'head_injury')] <- lapply(data.syn[,  c('Geschlecht', 'Age_Categ', 'head_injury')],
                                                                  factor)

numeric_vars <- select_if(data.syn, is.numeric)

data.syn.caseid <- data.syn # to keep for mixed modelling

numeric_vars <- names(data.syn.caseid)[sapply(data.syn.caseid, is.numeric)]
not_numeric <- names(data.syn.caseid)[!sapply(data.syn.caseid, is.numeric)]
data.syn.caseid.scaled <- cbind( data.syn.caseid[, not_numeric],
                                 data.frame(scale(data.syn.caseid[, numeric_vars])) )

# Cross-validation

set.seed(2049)

frm_full <- as.formula(paste0('flag~(1|research_case_id)+',
                              paste(names(data.syn.caseid.scaled)[17:18], collapse = '+'), '+', # age, icu, surgery nr
                              paste(names(data.syn.caseid.scaled)[grep('mean|n_|sd',  # looking at mean, n and sd only
                                                                       names(data.syn.caseid.scaled))], 
                                    collapse = "+"), '+',
                              paste(names(data.syn.caseid.scaled)[3:15], collapse = '+'))) # factors

glm_mod_full <- glmer(frm_full, # the random effect
                      data = data.syn.caseid.scaled, family = 'binomial',
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # how it looks - without CV

summary(glm_mod_full)

AIC_full <- summary(glm_mod_full)$AICtab[[1]] # 4236.29

pred_full <- cross_val(data.syn.caseid.scaled, k = 10,formula = frm_full)

test <- roc(data.syn.caseid.scaled$flag, pred_full, levels = c("No", "Yes"), percent = TRUE)
plot.roc(test, main="ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA)

roc_details <- data.frame(pred = pred_full, obs = data.syn.caseid.scaled$flag)
save(roc_details, file = 'roc_details_all_imputed.RData') # all values, not just complete
save(glm_mod_full, file = 'glmm_all_imputed.RData')

# imputed complete cases
load("df_all_imputed_complete_cases.Rdata")

data.syn <- df # with missing data

# count variables for the three types of data. Very highly correlated
count_variables_vit <- names(data.syn)[grep('_n', names(data.syn))][1:4]
count_variables_bga <- names(data.syn)[grep('_n', names(data.syn))][4:6]
count_variables_blut <- names(data.syn)[grep('_n', names(data.syn))][7:19]

data.syn$n_vit <- apply(data.syn[ , count_variables_vit], 1, median)
data.syn$n_bga <- apply(data.syn[ , count_variables_bga], 1, median)
data.syn$n_blut <- apply(data.syn[ , count_variables_blut], 1, median)

# remove # of observations for each measurement
data.syn <- data.syn %>% 
  dplyr::select(-c(count_variables_bga, count_variables_blut, count_variables_vit))

# removing the median as we already have the mean and PCTB
data.syn <- data.syn %>% dplyr::select(-matches("median"))%>% 
  dplyr::select(-matches("PCTB"))

# keeping age and gender and converting to factors
data.syn <- data.syn %>% 
  dplyr::select(-c(Asa_vor_unfall, Primaer_oder_zuweisung, Trauma_mechanismus, Schwangerschaft))

# converting variables into factors
factor_var_01 <- c('flag', 'icu', "PTZEIT","HUFH","PCT",
                   "LYM", "NEU", "LDH","GGT" ,
                   "IMGR","IMGRA" ,"MYO","TZI")

data.syn <- data.syn %>%
  mutate(across(factor_var_01, 
                ~factor(ifelse(.x == "1","Yes","No"))))

data.syn[, c('Geschlecht', 'Age_Categ', 'head_injury')] <- lapply(data.syn[,  c('Geschlecht', 'Age_Categ', 'head_injury')],
                                                   factor)

numeric_vars <- select_if(data.syn, is.numeric)

data.syn.caseid <- data.syn # to keep for mixed modelling

numeric_vars <- names(data.syn.caseid)[sapply(data.syn.caseid, is.numeric)]
not_numeric <- names(data.syn.caseid)[!sapply(data.syn.caseid, is.numeric)]
data.syn.caseid.scaled <- cbind( data.syn.caseid[, not_numeric],
                                 data.frame(scale(data.syn.caseid[, numeric_vars])) )

# Cross-validation

set.seed(2049)

frm_full <- as.formula(paste0('flag~(1|research_case_id)+',
                         paste(names(data.syn.caseid.scaled)[17:18], collapse = '+'), '+', # age, icu, surgery nr
                         paste(names(data.syn.caseid.scaled)[grep('mean|n_|sd',  # looking at mean, n and sd only
                                                                  names(data.syn.caseid.scaled))], 
                               collapse = "+"), '+',
                         paste(names(data.syn.caseid.scaled)[3:15], collapse = '+'))) # factors

glm_mod_full <- glmer(frm_full, # the random effect
                  data = data.syn.caseid.scaled, family = 'binomial',
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # how it looks - without CV


summary(glm_mod_full)

AIC_full <- summary(glm_mod_full)$AICtab[[1]] # 4236.29

pred_full <- cross_val(data.syn.caseid.scaled, k = 10,formula = frm_full)

test <- roc(data.syn.caseid.scaled$flag, pred_full, levels = c("No", "Yes"), percent = TRUE)
plot.roc(test, main="ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA)


roc_details <- data.frame(pred = pred_full, obs = data.syn.caseid.scaled$flag)
save(roc_details, file = 'roc_details_all_imputed_complete_only.RData')
save(glm_mod_full, file = 'glmm_all_imputed_complete_only.RData')

# unimputed
load("unimputed_complete_cases.RData")

data.syn <- df_complete # with missing data

# count variables for the three types of data. Very highly correlated
count_variables_vit <- names(data.syn)[grep('_n', names(data.syn))][1:4]
count_variables_bga <- names(data.syn)[grep('_n', names(data.syn))][4:6]
count_variables_blut <- names(data.syn)[grep('_n', names(data.syn))][7:19]

data.syn$n_vit <- apply(data.syn[ , count_variables_vit], 1, median)
data.syn$n_bga <- apply(data.syn[ , count_variables_bga], 1, median)
data.syn$n_blut <- apply(data.syn[ , count_variables_blut], 1, median)

# remove # of observations for each measurement
data.syn <- data.syn %>% 
  dplyr::select(-c(count_variables_bga, count_variables_blut, count_variables_vit))

# removing the median as we already have the mean and PCTB
data.syn <- data.syn %>% dplyr::select(-matches("median"))%>% 
  dplyr::select(-matches("PCTB"))

# keeping age and gender and converting to factors
data.syn <- data.syn %>% 
  dplyr::select(-c(Asa_vor_unfall, Primaer_oder_zuweisung, Trauma_mechanismus, Schwangerschaft))

# converting variables into factors
factor_var_01 <- c('flag', 'icu', "PTZEIT","HUFH","PCT",
                   "LYM", "NEU", "LDH","GGT" ,
                   "IMGR","IMGRA" ,"MYO","TZI")

data.syn <- data.syn %>%
  mutate(across(factor_var_01, 
                ~factor(ifelse(.x == "1","Yes","No"))))

data.syn[, c('Geschlecht', 'Age_Categ', 'head_injury')] <- lapply(data.syn[,  c('Geschlecht', 'Age_Categ', 'head_injury')],
                                                                  factor)

numeric_vars <- select_if(data.syn, is.numeric)

data.syn.caseid <- data.syn # to keep for mixed modelling

numeric_vars <- names(data.syn.caseid)[sapply(data.syn.caseid, is.numeric)]
not_numeric <- names(data.syn.caseid)[!sapply(data.syn.caseid, is.numeric)]
data.syn.caseid.scaled <- cbind( data.syn.caseid[, not_numeric],
                                 data.frame(scale(data.syn.caseid[, numeric_vars])) )

# Cross-validation

set.seed(2049)

frm_full <- as.formula(paste0('flag~(1|research_case_id)+',
                              paste(names(data.syn.caseid.scaled)[16:18], collapse = '+'), '+', # icu, surgery nr
                              paste(names(data.syn.caseid.scaled)[grep('mean|n_|sd',  # looking at mean, n and sd only
                                                                       names(data.syn.caseid.scaled))], 
                                    collapse = "+"), '+',
                              paste(names(data.syn.caseid.scaled)[3:14], collapse = '+'))) # factors

glm_mod_full <- glmer(frm_full, # the random effect
                      data = data.syn.caseid.scaled, family = 'binomial',
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) # how it looks - without CV

summary(glm_mod_full)

AIC_full <- summary(glm_mod_full)$AICtab[[1]] # 4236.29

pred_full <- cross_val(data.syn.caseid.scaled, k = 10,formula = frm_full)

test <- roc(data.syn.caseid.scaled$flag, pred_full, levels = c("No", "Yes"), percent = TRUE)
plot.roc(test, main="ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA)


roc_details <- data.frame(pred = pred_full, obs = data.syn.caseid.scaled$flag)
save(roc_details, file = 'roc_details_unimputed_complete_only.RData')
save(glm_mod_full, file = 'glmm_unimputed_complete_only.RData')
