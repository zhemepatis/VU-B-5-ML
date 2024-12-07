library(rpart.plot)
library(rpart)

# source("script/analysis/prediction-plot.r")
# source("script/analysis/prediction-stats.r")
# source("script/analysis/roc-curve.r")

apply_decision_tree <- function(training_set, validation_set, plot_decision_tree = FALSE) {
  training_set$label <- as.factor(training_set$label)
  validation_set$label <- as.factor(validation_set$label)
  
  # apmokom modeli naudojant pilna duomenu aibe
  model <- rpart(label ~ ., data = training_set)
  if (plot_decision_tree) {
    rpart.plot(model, type = 1, extra = "auto")
  }
  
  # validuojam modeli
  target_cols <- setdiff(names(validation_set), "label")
  prediction <- predict(model, newdata = validation_set[, target_cols], type = 'class')
  prediction_prob <- predict(model, newdata = validation_set[, target_cols], type = 'prob')
  
  # surenkam rezultatus
  results <- list(
    prediction = prediction,
    prediction_prob = prediction_prob
  )
  
  return(results)
}