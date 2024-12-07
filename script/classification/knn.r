library(class)

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

  results <- list(
    prediction = prediction,
    prediction_prob = prediction_prob_matrix
  )
  
  return(results)
}