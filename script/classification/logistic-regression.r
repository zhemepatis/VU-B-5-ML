apply_logistic_regression <- function(training_set, validation_set, threshold = 0.5, maxit = 1000, epsilon = 1e-8) {
  # Ensure the 'label' column is a factor for logistic regression
  training_set$label <- as.factor(training_set$label)
  validation_set$label <- as.factor(validation_set$label)
  
  # Prepare data (remove 'label' column, convert to matrix for features)
  x_train <- training_set[, -which(names(training_set) == "label")]
  y_train <- training_set$label  # 'label' column is the target
  x_valid <- validation_set[, -which(names(validation_set) == "label")]
  
  # Fit a logistic regression model with fixed settings, including maxit and epsilon
  model <- glm(label ~ ., data = training_set, family = binomial(link = "logit"),
               control = list(maxit = maxit, epsilon = epsilon))
  
  # Make predictions on the validation set
  prediction_prob_positive <- predict(model, newdata = validation_set, type = "response")
  
  # Prepare probability matrix for both classes
  prediction_prob <- cbind(
    "0" = 1 - prediction_prob_positive,
    "2" = prediction_prob_positive
  )
  
  # Apply threshold to make class predictions
  prediction <- ifelse(prediction_prob_positive > threshold, "2", "0")
  
  # Return results
  results <- list(
    prediction = prediction,
    prediction_prob = prediction_prob
  )
  
  return(results)
}
