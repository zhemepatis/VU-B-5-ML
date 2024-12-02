library(pROC)

roc_curve <- function(validation_set, prediction_prob, positive_class = "1", title = "ROC kreivė") {
  validation_set$label <- factor(validation_set$label, levels = colnames(prediction_prob))

  actual <- validation_set$label
  prob_positive <- prediction_prob[, positive_class]

  valid_idx <- !is.na(actual) & !is.na(prob_positive)
  actual <- actual[valid_idx]
  prob_positive <- prob_positive[valid_idx]
  
  roc_obj <- roc(actual, prob_positive, levels = rev(levels(actual)))

  ggroc(roc_obj) +
    ggtitle(title) +
    xlab("1 - Specificity (False Positive Rate)") +
    ylab("Sensitivity (True Positive Rate)") +
    theme_minimal() +
    geom_abline(linetype = "dashed", color = "gray")
}

get_auc <- function(validation_set, prediction_prob, positive_class = "1") {
  if (!is.factor(validation_set$label)) {
    validation_set$label <- as.factor(validation_set$label)
  }
  
  prob_positive <- prediction_prob[, positive_class]
  
  roc_obj <- roc(validation_set$label, prob_positive, levels = rev(levels(validation_set$label)))
  
  auc_value <- auc(roc_obj)
  return(auc_value)
}