library(class)  # For KNN
source("script/analysis/prediction-plot.r")
source("script/analysis/prediction-stats.r")
source("script/analysis/roc-curve.r")

apply_knn <- function(training_set, validation_set) {
  training_set$label <- as.factor(training_set$label)
  validation_set$label <- as.factor(validation_set$label)

  train_features <- training_set[, setdiff(names(training_set), "label")]
  train_labels <- training_set$label
  validation_features <- validation_set[, setdiff(names(validation_set), "label")]
  
  prediction <- knn(
    train = train_features, 
    test = validation_features, 
    cl = train_labels, 
    k = 1, 
    prob = TRUE
  )

  prediction_prob <- attr(prediction, "prob")
  prediction_prob <- ifelse(
    prediction == levels(train_labels)[1], 
    prediction_prob, 
    1 - prediction_prob
  )
  
  prediction_prob_matrix <- matrix(0, nrow = length(prediction), ncol = length(levels(train_labels)))
  colnames(prediction_prob_matrix) <- levels(train_labels)
  for (i in seq_along(prediction)) {
    prediction_prob_matrix[i, as.character(prediction[i])] <- prediction_prob[i]
  }
  
  # Results
  results <- list(
    prediction = prediction,
    prediction_prob = prediction_prob_matrix
  )
  
  return(results)
}

# nesuspausta, pilna duomenu aibe
results <- apply_knn(training_set, validation_set)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

validation_set_reduced <- perform_umap(validation_set)

get_stats(validation_set, prediction)
plot_predictions(validation_set_reduced, prediction, "KNN klasifikavimo rezultatai pilnai aibei")
roc_curve(validation_set, prediction_prob, positive_class = "2", "KNN ROC kreivė pilnai aibei")
get_auc(validation_set, prediction_prob, "2")

# suspausta, atrinkta duomenu aibe
results <- apply_knn(training_set_2d, validation_set_2d)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

get_stats(validation_set_2d, prediction)
plot_predictions(validation_set_2d, prediction, "KNN klasifikavimo rezultatai apribotai suspaustai aibei")
roc_curve(validation_set_2d, prediction_prob, positive_class = "2", "KNN ROC kreivė apribotai suspaustai aibei")
get_auc(validation_set_2d, prediction_prob, "2")