library(randomForest)

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