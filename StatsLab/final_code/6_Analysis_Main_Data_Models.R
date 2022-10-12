# ROC plotting and other analysis of the models (glmm and ml) run for the main dataset

library(plotROC)
library(pROC)
library(caret)
library(likert)
library(gtsummary)

# analysis of main data models

# GLMM
load('glmm_unimputed_complete_only.RData')
load('roc_details_unimputed_complete_only.RData')

# make sure to talk about problem of dimensionality and identifiability

unimp_roc <- roc(roc_details$obs, roc_details$pred, levels = c("No", "Yes"), percent = TRUE)

load('roc_details_all_imputed.RData') # all values, not just complete
load('glmm_all_imputed.RData')

imp_roc <- roc(roc_details$obs, roc_details$pred, levels = c("No", "Yes"), percent = TRUE)

load('roc_details_all_imputed_complete_only.RData')
load('glmm_all_imputed_complete_only.RData')
sink("glmm_imp_cc_summary.txt")
print(summary(glm_mod_full))
sink()  # returns output to the console

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

roc_data_lgr <- roc(penalised_lgr$pred$obs, penalised_lgr$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_xgb <- roc(fit_xg$pred$obs, fit_xg$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_rfs <- roc(fit_rf$pred$obs, fit_rf$pred$Yes, levels = c("No", "Yes"), percent = TRUE)

pdf(file = "unimputed_auc_roc.pdf", width = 10, height = 9)

par(cex.main=1, cex.lab = 1)
plot.roc(roc_data_lgr, main="10-fold CV ROC", col = "red", print.auc = FALSE,  legacy.axes = TRUE, add = FALSE, asp = NA, lwd = 2, lty = 1, cex.lab = 1.35, cex.main = 2)
plot.roc(roc_data_xgb, main="10-fold CV ROC", col = "darkgreen", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 1 )
plot.roc(roc_data_rfs, main="10-fold CV ROC", col = "blue", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 1)

legend("topleft",
       legend=c(paste("Unimputed - Logistic", ", AUC: 0.54", sep = ""), 
                paste('Unimputed - XGBoost', ", AUC: 0.61", sep = ""),
                paste('Unimputed - Random Forest', ", AUC: 0.59", sep = "")),
       col=c("red", "darkgreen", "blue"),
       lwd= 3, cex = 1, xpd = FALSE, lty = c(rep(1, times = 3)), bty = "n", x.intersp = 0.15, text.width = 0.045, y.intersp = 0.75)

load('imputed_complete_cases_rf.RData')
load('imputed_complete_cases_xg.RData')
load('imputed_complete_cases_penalised_lgr.RData')
roc_data_lgr_imp <- roc(penalised_lgr$pred$obs, penalised_lgr$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_xgb_imp <- roc(fit_xg$pred$obs, fit_xg$pred$Yes, levels = c("No", "Yes"), percent = TRUE)
roc_data_rfs_imp <- roc(fit_rf$pred$obs, fit_rf$pred$Yes, levels = c("No", "Yes"), percent = TRUE, smooth = TRUE)

pdf(file = "imputed_auc_roc.pdf", width = 10, height = 9)

par(cex.main=1, cex.lab = 1)
plot.roc(roc_data_lgr_imp, main="10-fold CV ROC", col = "red", print.auc = FALSE,  legacy.axes = TRUE, add = FALSE, asp = NA, lwd = 2, lty = 1, cex.lab = 1.35, cex.main = 2)
plot.roc(roc_data_xgb_imp, main="10-fold CV ROC", col = "darkgreen", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 1 )
plot.roc(roc_data_rfs_imp, main="10-fold CV ROC", col = "blue", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 1)

legend("topleft",
       legend=c(paste("Imputed - Logistic", ", AUC: 0.57", sep = ""),
                paste('Imputed - XGBoost', ", AUC: 0.64", sep = ""), 
                paste('Imputed - Random Forest', ", AUC: 0.68", sep = "")),
       col=c("red", "darkgreen", "blue"),
       lwd= 3, cex = 1, xpd = FALSE, lty = c(rep(1, times = 3)), bty = "n", x.intersp = 0.15, text.width = 0.045, y.intersp = 0.75)

# writing AUC_ROC graphs to pdf

pdf(file = "main_auc_roc.pdf", width = 10, height = 9)

par(cex.main=1, cex.lab = 1)
plot.roc(roc_data_lgr, main="10-fold CV ROC", col = "red", print.auc = FALSE,  legacy.axes = TRUE, add = FALSE, asp = NA, lwd = 2, lty = 1, cex.lab = 1.35, cex.main = 2)
plot.roc(roc_data_xgb, main="10-fold CV ROC", col = "darkgreen", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 1 )
plot.roc(roc_data_rfs, main="10-fold CV ROC", col = "blue", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 1)
plot.roc(roc_data_lgr_imp, main="10-fold CV ROC", col = "red", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA,  lwd = 2, lty = 2)
plot.roc(roc_data_xgb_imp, main="10-fold CV ROC", col = "darkgreen", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 2, lty = 2)
plot.roc(roc_data_rfs_imp, main="10-fold CV ROC", col = "blue", print.auc = FALSE,  legacy.axes = TRUE, add = TRUE, asp = NA, lwd = 3, lty = 2)

legend("topleft",
       legend=c(paste("Unimputed - Logistic", ", AUC: 0.54", sep = ""), 
                paste('Unimputed - XGBoost', ", AUC: 0.61", sep = ""),
                paste('Unimputed - Random Forest', ", AUC: 0.60", sep = ""), 
                paste("Imputed - Logistic", ", AUC: 0.57", sep = ""),
                paste('Imputed - XGBoost', ", AUC: 0.64", sep = ""), 
                paste('Imputed - Random Forest', ", AUC: 0.69", sep = "")),
       col=c("red", "darkgreen", "blue", "red", "darkgreen", "blue"),
       lwd= 3, cex = 1, xpd = FALSE, lty = c(rep(1, times = 3), rep(2, times = 3)), bty = "n", x.intersp = 0.15, text.width = 0.045, y.intersp = 0.75)

### feature importance

caret_imp <- varImp(fit_rf, scale = FALSE)
pdf(file = "rf_main_var_imp.pdf", width = 10, height = 9)
plot(caret_imp, top=10)
dev.off()


caret_imp <- varImp(fit_xg, scale = FALSE)
pdf(file = "xgb_main_var_imp.pdf", width = 10, height = 9)
plot(caret_imp, top=10)
dev.off()

load('unimputed_xg.RData')
load('unimputed_rf.RData')

caret_imp <- varImp(fit_rf, scale = FALSE)
pdf(file = "rf_main_var_unimp.pdf", width = 10, height = 9)
plot(caret_imp, top=10)
dev.off()


caret_imp <- varImp(fit_xg, scale = FALSE)
pdf(file = "xgb_main_var_unimp.pdf", width = 10, height = 9)
plot(caret_imp, top=10)
dev.off()

#### confusion matrix

cm <- confusionMatrix(fit_rf$finalModel$predicted, fit_rf$trainingData$flag) #create a confusion matrix
cm_d <- as.data.frame(cm$table) # extract the confusion matrix values as data.frame
cm_st <-data.frame(cm$overall) # confusion matrix statistics as data.frame
cm_st$cm.overall <- round(cm_st$cm.overall,2) # round the values
cm_d$diag <- cm_d$Prediction == cm_d$Reference # Get the Diagonal
cm_d$ndiag <- cm_d$Prediction != cm_d$Reference # Off Diagonal     
cm_d[cm_d == 0] <- NA # Replace 0 with NA for white tiles
cm_d$Reference <-  reverse.levels(cm_d$Reference) # diagonal starts at top left
cm_d$ref_freq <- cm_d$Freq * ifelse(is.na(cm_d$diag),-1,1)

pdf(file = "conf_mat_rf_main.pdf", width = 10, height = 9)

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

xtable::xtable(cm , caption = NULL, label = NULL,
       align = NULL, digits = NULL, display = NULL)

# obtain list of coefficients from penalised logistic regression
library(xtable)
coefs <- as.matrix(coef(penalised_lgr$finalModel, penalised_lgr$finalModel$lambdaOpt))
coefs <- cbind(rownames(coefs), coefs)
coefs <- coefs[order(abs(as.numeric(coefs[, 2]))), ]
row.names(coefs) <- NULL
coefs[, 2] <- round(as.numeric(coefs[, 2]), 3)

xtable(coefs, digits = 2)

tbl_regression(penalised_lgr$finalModel)
