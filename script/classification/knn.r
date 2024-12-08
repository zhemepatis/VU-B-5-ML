library(class)

apply_knn <- function(training_set, validation_set, k = 1) {
  # Ensure the labels are factors
  training_set$label <- as.factor(training_set$label)
  validation_set$label <- as.factor(validation_set$label)
  
  # Separate features and labels
  train_features <- training_set[, setdiff(names(training_set), "label")]
  train_labels <- training_set$label
  validation_features <- validation_set[, setdiff(names(validation_set), "label")]
  
  # Apply the kNN algorithm with probabilities
  prediction <- knn(
    train = train_features, 
    test = validation_features, 
    cl = train_labels, 
    k = k, 
    prob = TRUE
  )
  
  # Extract the probabilities
  prediction_prob <- attr(prediction, "prob")
  
  # Create a probability matrix
  prediction_prob_matrix <- matrix(0, nrow = length(prediction), ncol = length(levels(train_labels)))
  colnames(prediction_prob_matrix) <- levels(train_labels)
  
  # Fill in the probability matrix
  for (i in seq_along(prediction)) {
    predicted_class <- as.character(prediction[i])
    prob_value <- prediction_prob[i]
    if (predicted_class == levels(train_labels)[1]) {
      prediction_prob_matrix[i, predicted_class] <- prob_value
    } else {
      prediction_prob_matrix[i, predicted_class] <- 1 - prob_value
    }
  }
  
  results <- list(
    prediction = prediction,
    prediction_prob = prediction_prob_matrix
  )
  
  return(results)
}
