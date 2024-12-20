library(rpart.plot)
library(rpart)

source("script/analysis/prediction-plot.r")
source("script/analysis/prediction-stats.r")
source("script/analysis/roc-curve.r")
source("script/classification/logistic-regression.r")
source("script/data-preparation/norm.r")
source("script/dimension-reduction/umap.r")


roc_curve <- function(data, prediction_prob, positive_class = "1", title = "ROC kreivė") {
  data$label <- factor(data$label, levels = colnames(prediction_prob))
  roc_obj <- roc(data$label, prediction_prob[, positive_class], levels = rev(levels(data$label)))
  
  print(
    ggroc(roc_obj) +
      ggtitle(title) +
      xlab("1 - Specificity (False Positive Rate)") +
      ylab("Sensitivity (True Positive Rate)") +
      theme_minimal() +
      geom_abline(linetype = "dashed", color = "gray")
  )
  
  auc <- auc(roc_obj)
  return(auc)
}

# normuojam duomenis
normalization_result <- normalize_sets(training_set, test_set)
training_set <- normalization_result$main_set
test_set <- normalization_result$secondary_set

target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")
training_set_2d <- training_set[, target_cols]
test_set_2d <- test_set[, target_cols]

# nesuspausta, pilna duomenu aibe
lr_results <- apply_logistic_regression(training_set, test_set, threshold = 0.5, maxit = 1000, epsilon = 1e-8)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

test_set_reduced <- perform_umap(test_set, set_seed = TRUE)

plot_predictions(test_set_reduced, prediction, "Logistinės regresijos klasifikavimo rezultatai pilnai aibei")





# apirbota, suspausta duomenu aibe
# training_set_2d <- perform_umap(training_set_2d, set_seed = TRUE)
# test_set_2d <- perform_umap(test_set_2d, set_seed = TRUE)
# 
# results <- apply_logistic_regression(training_set_2d, test_set_2d, threshold = 0.5, maxit = 1000, epsilon = 1e-8)
# prediction <- results$prediction
# prediction_prob <- results$prediction_prob
# 
# plot_predictions(test_set_2d, prediction, "Logistinės regresijos klasifikavimo rezultatai apribotai suspaustai aibei")
# auc <- roc_curve(test_set_2d, prediction_prob, positive_class = "2", "Logstinės regresijos ROC kreivė apribotai suspaustai aibei")
# print(auc)