source("script/classification/test/random-forest-test.r")
source("script/classification/test/decision-tree-test.r")
source("script/classification/test/logistic-regression-test.r")
library(pROC)


dt_results$prediction_prob <- as.numeric(dt_results$prediction_prob)
lr_results$prediction_prob <- as.numeric(lr_results$prediction_prob)
rf_results$prediction_prob <- as.numeric(rf_results$prediction_prob)

dt_results_2d$prediction_prob <- as.numeric(dt_results_2d$prediction_prob)
lr_results_2d$prediction_prob <- as.numeric(lr_results_2d$prediction_prob)
rf_results_2d$prediction_prob <- as.numeric(rf_results_2d$prediction_prob)


# pilna
roc_dt <- roc(test_set$label, dt_results$prediction_prob, plot = FALSE)
plot(roc_dt, col = "blue", main = "ROC kreivės pilnai duomenų aibei", lwd = 2)
auc_dc <- auc(roc_dt)

roc_lr <- roc(test_set$label, lr_results$prediction_prob, plot = FALSE)
lines(roc_lr, col = "green", lwd = 2)
auc_lr <- auc(roc_lr)

roc_rf <- roc(test_set$label, rf_results$prediction_prob, plot = FALSE)
lines(roc_rf, col = "red", lwd = 2)
auc_rf <- auc(roc_rf)

legend("bottomright", legend = c(
  paste("Sprendimų medis: AUC =", round(auc_dc, 3)),
  paste("Logistinė regresija: AUC =", round(auc_lr, 3)),
  paste("Atsitiktinis miškas: AUC =", round(auc_rf, 3))
), col = c("blue", "green", "red"), lwd = 2, cex = 0.8)



# dvimate
roc_dt_2d <- roc(test_set_2d$label, dt_results_2d$prediction_prob, plot = FALSE)
plot(roc_dt_2d, col = "blue", main = "ROC kreivės dvimatei duomenų aibei", lwd = 2)
auc_dc_2d <- auc(roc_dt_2d)

roc_lr_2d <- roc(test_set_2d$label, lr_results_2d$prediction_prob, plot = FALSE)
lines(roc_lr_2d, col = "green", lwd = 2)
auc_lr_2d <- auc(roc_lr_2d)

roc_rf_2d <- roc(test_set_2d$label, rf_results_2d$prediction_prob, plot = FALSE)
lines(roc_rf_2d, col = "red", lwd = 2)
auc_rf_2d <- auc(roc_rf_2d)

legend("bottomright", legend = c(
  paste("Sprendimų medis: AUC =", round(auc_dc_2d, 3)),
  paste("Logistinė regresija: AUC =", round(auc_lr_2d, 3)),
  paste("Atsitiktinis miškas: AUC =", round(auc_rf_2d, 3))
), col = c("blue", "green", "red"), lwd = 2, cex = 0.8)
