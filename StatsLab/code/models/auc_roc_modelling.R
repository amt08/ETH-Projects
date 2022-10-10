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
library(ranger)
library(likert)
library(gtsummary)

# loading saved models

base::load('glm_aic.Rdata')
base::load('penalised_lgr.Rdata')
base::load('xgb_model.Rdata')
base::load('rfs.Rdata')

# roc for the different models

roc_data_lgr <- roc(penalised_lgr$pred$obs, penalised_lgr$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_xgb <- roc(xgb_tune$pred$obs, xgb_tune$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_rfs <- roc(modellist$`500`$pred$obs, modellist$`500`$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
plot.roc(roc_data_lgr, main="5-fold CV ROC", col = "blue", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA, print.auc.y = 25, lwd = 3)
plot.roc(roc_data_xgb, main="5-fold CV ROC", col = "darkgreen", print.auc = TRUE,  legacy.axes = TRUE, add = TRUE, asp = NA, print.auc.y=35, lwd = 3 )
plot.roc(roc_data_rfs, main="5-fold CV ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = TRUE, asp = NA, print.auc.y= 45, lwd = 3)

legend("topleft",
       legend=c("Penalised logistic regression", 'XGBoost', 'Random Forests'),
       col=c("blue", "darkgreen", "red"),
       lwd= 4, cex = 0.9, xpd = FALSE, bty = "n")


# variable importance for XGBoost and RFs


caret_imp <- varImp(xgb_tune, scale = FALSE)
caret_imp
plot(caret_imp, top=10)


caret_imp <- varImp(modellist$`500`, scale = FALSE)
caret_imp
plot(caret_imp, top=10)

xgb.plot.shap(as.matrix(xgb_tune$trainingData))

#####################################################################
############################################# Main dataset

base::load('taru/penalised_lgr_imputed.RData')
base::load('taru/rf_imputed.RData')
base::load('taru/xg_imputed.RData')

base::load('taru/adn_xgb_model.Rdata')
base::load('taru/adn_penalised_lgr.Rdata')
base::load('taru/fit-final.Rdata')

base::load('taru/roc_details.RData')

rf <- fit_final$ranger

roc_data_lgr <- roc(penalised_lgr$pred$obs, penalised_lgr$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_xgb <- roc(fit_xg$pred$obs, fit_xg$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_rfs <- roc(fit_rf$pred$obs, fit_rf$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_lgr_icu <- roc(lgr$pred$obs, lgr$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_xgb_icu <- roc(xgb_tune$pred$obs, xgb_tune$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_rfs_icu <- roc(rf$pred$obs, rf$pred$Yes, levels = c("No", "Yes"), percent = TRUE, smooth = TRUE)
roc_glmm <- roc(roc_details$obs, roc_details$pred, levels = c("No", "Yes"), percent = TRUE, smooth = TRUE)

# writing AUC_ROC graphs to pdf

pdf(file = "icu_auc_roc.pdf", width = 10, height = 9)

par(cex.main=1, cex.lab = 1)
plot.roc(roc_data_lgr, main="10-fold CV ROC", col = "red", print.auc = FALSE,  legacy.axes = TRUE, add = FALSE, asp = NA, lwd = 2, lty = 1, cex.lab = 1.35, cex.main = 2)
plot.roc(roc_data_xgb, main="10-fold CV ROC", col = "darkgreen", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 1 )
plot.roc(roc_data_rfs, main="10-fold CV ROC", col = "blue", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 1)
plot.roc(roc_data_lgr_icu, main="10-fold CV ROC", col = "red", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA,  lwd = 2, lty = 2)
plot.roc(roc_data_xgb_icu, main="10-fold CV ROC", col = "darkgreen", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 2)
plot.roc(roc_data_rfs_icu, main="10-fold CV ROC", col = "blue", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 3, lty = 2)
plot.roc(roc_glmm, main="10-fold CV ROC", col = "orange", print.auc = FALSE,  legacy.axes = TRUE,
         add = TRUE, asp = NA, lwd = 3, lty = 1)

plot.roc(roc_glmm, main="10-fold CV ROC", col = "red", print.auc = TRUE,  legacy.axes = TRUE, add = FALSE, asp = NA, lwd = 2, lty = 1, cex.lab = 1.35, cex.main = 2)


legend("topleft",
       legend=c(paste("Main - Logistic", ", AUC: 0.56", sep = ""), paste('Main - XGBoost', ", AUC: 0.63", sep = ""),
                paste('Main - Random Forest', ", AUC: 0.67", sep = ""), paste('Main - GLMM', ", AUC: 0.67", sep = ""),
                paste("ICU - Logistic", ", AUC: 0.54", sep = ""),
                paste('ICU - XGBoost', ", AUC: 0.58", sep = ""), paste('ICU - Random Forest', ", AUC: 0.64", sep = "")),
       col=c("red", "darkgreen", "blue", "orange", "red", "darkgreen", "blue"),
       lwd= 3, cex = 1, xpd = FALSE, lty = c(rep(1, times = 4), rep(2, times = 3)), bty = "n", x.intersp = 0.15, text.width = 0.045, y.intersp = 0.75)


### feature importance
caret_imp <- varImp(modellist$`1000`, scale = FALSE)
pdf(file = "rf_icu_var_imp.pdf", width = 10, height = 9)
plot(caret_imp, top=10)
dev.off()


### feature importance
caret_imp <- varImp(xgb_tune, scale = FALSE)
pdf(file = "xgb_icu_var_imp.pdf", width = 10, height = 9)
plot(caret_imp, top=10)
dev.off()


#### confusion matrix

rf$bestTune$

cm <- confusionMatrix(rf$pred$pred,rf$pred$obs) #create a confusion matrix
cm_d <- as.data.frame(cm$table) # extract the confusion matrix values as data.frame
cm_st <-data.frame(cm$overall) # confusion matrix statistics as data.frame
cm_st$cm.overall <- round(cm_st$cm.overall,2) # round the values
cm_d$diag <- cm_d$Prediction == cm_d$Reference # Get the Diagonal
cm_d$ndiag <- cm_d$Prediction != cm_d$Reference # Off Diagonal     
cm_d[cm_d == 0] <- NA # Replace 0 with NA for white tiles
cm_d$Reference <-  reverse.levels(cm_d$Reference) # diagonal starts at top left
cm_d$ref_freq <- cm_d$Freq * ifelse(is.na(cm_d$diag),-1,1)

pdf(file = "conf_mat_xgb.pdf", width = 10, height = 9)

plt1 <-  ggplot(data = cm_d, aes(x = Prediction , y =  Reference, fill = Freq))+
        scale_x_discrete(position = "top") +
        geom_tile( data = cm_d,aes(fill = ref_freq)) +
        scale_fill_gradient2(guide = FALSE ,low="lightblue",high="skyblue", midpoint = 0,na.value = 'white') +
        geom_text(aes(label = Freq), color = 'black', size = 3)+
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              legend.position = "none",
              panel.border = element_blank(),
              plot.background = element_blank(),
              axis.line = element_blank(),
        )
plt1

dev.off()

# obtain list of coefficients from penalised logistic regression

coef(lgr$finalModel, lgr$finalModel$lambdaOpt)

tbl_regression(lgr$finalModel)
