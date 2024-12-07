apply_logistic_regression <- function(training_set, validation_set) {
  training_set$label <- as.factor(training_set$label)
  validation_set$label <- as.factor(validation_set$label)
  
  model <- glm(label ~ ., data = training_set, family = binomial)
  
  prediction_prob_positive <- predict(model, newdata = validation_set, type = 'response')
  
  prediction_prob <- cbind(
    "0" = 1 - prediction_prob_positive,
    "2" = prediction_prob_positive
  )
  
  prediction <- ifelse(prediction_prob_positive > 0.5, "2", "0")
  
  results <- list(
    prediction = prediction,
    prediction_prob = prediction_prob
  )
  
  return(results)
}