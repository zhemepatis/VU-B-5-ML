library(pROC)

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