library(randomForest)  # For Random Forest
source("script/analysis/prediction-plot.r")
source("script/analysis/prediction-stats.r")
source("script/analysis/roc-curve.r")

apply_random_forest <- function(training_set, validation_set) {
  training_set$label <- as.factor(training_set$label)
  validation_set$label <- as.factor(validation_set$label)
  
  model <- randomForest(label ~ ., data = training_set)
  
  target_cols <- setdiff(names(validation_set), "label")
  
  prediction <- predict(model, newdata = validation_set[, target_cols])
  
  prediction_prob <- predict(model, newdata = validation_set[, target_cols], type = "prob")
  
  results <- list(
    prediction = prediction,
    prediction_prob = prediction_prob
  )
  
  return(results)
}

# nesuspausta, pilna duomenu aibe
results <- apply_random_forest(training_set, validation_set)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

validation_set_reduced <- perform_umap(validation_set)

get_stats(validation_set, prediction)
plot_predictions(validation_set_reduced, prediction, "Random Forest klasifikavimo rezultatai pilnai aibei")
roc_curve(validation_set, prediction_prob, positive_class = "2", "Random Forest ROC kreivė pilnai aibei")
get_auc(validation_set, prediction_prob, "2")


# suspausta, atrinkta duomenu aibe
results <- apply_random_forest(training_set_2d, validation_set_2d)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

get_stats(validation_set_2d, prediction)
plot_predictions(validation_set_2d, prediction, "Random Forest klasifikavimo rezultatai apribotai suspaustai aibei")
roc_curve(validation_set_2d, prediction_prob, positive_class = "2", "Random Forest ROC kreivė apribotai suspaustai aibei")
get_auc(validation_set_2d, prediction_prob, "2")
