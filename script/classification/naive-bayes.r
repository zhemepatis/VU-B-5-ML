library(e1071)
source("script/analysis/prediction-plot.r")
source("script/analysis/prediction-stats.r")
source("script/analysis/roc-curve.r")

apply_naive_bayes <- function(training_set, validation_set) {
  training_set$label <- as.factor(training_set$label)
  validation_set$label <- as.factor(validation_set$label)
  
  model <- naiveBayes(label ~ ., data = training_set)
  
  target_cols <- setdiff(names(validation_set), "label")
  prediction_prob <- predict(model, newdata = validation_set[, target_cols], type = "raw")
  
  prediction <- predict(model, newdata = validation_set[, target_cols], type = "class")
  
  results <- list(
    prediction = prediction,
    prediction_prob = prediction_prob
  )
  
  return(results)
}

# Full dataset
results <- apply_naive_bayes(training_set, validation_set)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

validation_set_reduced <- perform_umap(validation_set)

get_stats(validation_set, prediction)
plot_predictions(validation_set_reduced, prediction, "Naive Bayes klasifikavimo rezultatai pilnai aibei")
roc_curve(validation_set, prediction_prob, positive_class = "2", "Naive Bayes ROC kreivė pilnai aibei")
get_auc(validation_set, prediction_prob, "2")

# Reduced dataset
results <- apply_naive_bayes(training_set_2d, validation_set_2d)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

get_stats(validation_set_2d, prediction)
plot_predictions(validation_set_2d, prediction, "Naive Bayes klasifikavimo rezultatai apribotai suspaustai aibei")
roc_curve(validation_set_2d, prediction_prob, positive_class = "2", "Naive Bayes ROC kreivė apribotai suspaustai aibei")
get_auc(validation_set_2d, prediction_prob, "2")
