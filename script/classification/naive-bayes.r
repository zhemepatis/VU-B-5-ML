library(e1071)

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