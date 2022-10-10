library(plotROC)
library(pROC)

# analysis of main data models

setwd("~/Desktop/Data Folder - StatsLab - May 21")


# GLMM
load('glmm_unimputed_complete_only.RData')
load('roc_details_unimputed_complete_only.RData')

# make sure to talk about problem of dimensionality
# and identifiability

unimp_roc <- roc(roc_details$obs, roc_details$pred, levels = c("No", "Yes"), percent = TRUE)


load('roc_details_all_imputed.RData') # all values, not just complete
load('glmm_all_imputed.RData')

imp_roc <- roc(roc_details$obs, roc_details$pred, levels = c("No", "Yes"), percent = TRUE)


load('roc_details_all_imputed_complete_only.RData')
load('glmm_all_imputed_complete_only.RData')

imp_complete_roc <- roc(roc_details$obs, roc_details$pred, levels = c("No", "Yes"), percent = TRUE)

pdf(file = "glmm_auc_roc.pdf", width = 10, height = 9)

par(cex.main=1, cex.lab = 1)
plot.roc(imp_complete_roc, main="10-fold CV ROC", col = "red", print.auc = FALSE,  legacy.axes = TRUE, add = FALSE, asp = NA, lwd = 2, lty = 1, cex.lab = 1.35, cex.main = 2)
plot.roc(imp_roc, main="10-fold CV ROC", col = "darkgreen", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 1 )
plot.roc(unimp_roc, main="10-fold CV ROC", col = "blue", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 1)


legend("topleft",
       legend=c(paste("Imputed (complete cases)", ", AUC: 0.68", sep = ""), 
                paste('Imputed (with missing)',", AUC: 0.68", sep = ""),
                paste('Unimputed (complete cases)', ", AUC: 0.58", sep = "")),
       col=c("red", "darkgreen", "blue"),
       lwd= 3, cex = 1, xpd = FALSE, lty = 1, bty = "n", x.intersp = 0.15, text.width = 0.045, y.intersp = 0.75)
dev.off()

# machine learning for main data

load('unimputed_xg.RData')
load('unimputed_rf.RData')
load('unimputed_penalised_regression.RData')
#load('imputed_complete_cases_xg.RData')



roc_data_lgr <- roc(penalised_lgr$pred$obs, penalised_lgr$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_xgb <- roc(fit_xg$pred$obs, fit_xg$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_rfs <- roc(fit_rf$pred$obs, fit_rf$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
load('imputed_complete_cases_rf.RData')
load('imputed_complete_cases_penalised_lgr.RData')
roc_data_lgr_imp <- roc(penalised_lgr$pred$obs, penalised_lgr$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_xgb_icu <- roc(fit_xg$pred$obs, fit_xg$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_rfs_icu <- roc(fit_rf$pred$obs, fit_rf$pred$Yes, levels = c("No", "Yes"), percent = TRUE, smooth = TRUE)

# writing AUC_ROC graphs to pdf

pdf(file = "main_auc_roc.pdf", width = 10, height = 9)

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


