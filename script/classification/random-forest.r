library(randomForest)

apply_random_forest <- function(training_set, validation_set, ntree = 500, mtry = NULL) {
  training_set$label <- as.factor(training_set$label)
  validation_set$label <- as.factor(validation_set$label)
  
  if (is.null(mtry)) {
    p <- ncol(training_set) - 1
    mtry <- floor(sqrt(p))
  }
  
  model <- randomForest(label ~ ., data = training_set, ntree = ntree, mtry = mtry)
  
  oob_error_rate <- model$err.rate[ntree, "OOB"]
  print(sprintf("OOB Error Rate: %.2f%%", oob_error_rate * 100))
  
  target_cols <- setdiff(names(validation_set), "label")
  
  prediction <- predict(model, newdata = validation_set[, target_cols])
  
  prediction_prob <- predict(model, newdata = validation_set[, target_cols], type = "prob")
  
  results <- list(
    prediction = prediction,
    prediction_prob = prediction_prob
  )
  
  return(results)
}