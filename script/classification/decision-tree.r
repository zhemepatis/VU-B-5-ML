library(rpart.plot)
library(rpart)
source("script/analysis/prediction_plot.r")
source("script/analysis/prediction-stats.r")
source("script/analysis/roc-curve.r")

apply_decision_tree <- function(training_set, validation_set) {
  training_set$label <- as.factor(training_set$label)
  validation_set$label <- as.factor(validation_set$label)
  
  # apmokom modeli naudojant pilna duomenu aibe
  model <- rpart(label ~ ., data = training_set)
  rpart.plot(model, type = 1, extra = "auto")
  
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


# nesuspausta, pilna duomenu aibe
results <- apply_decision_tree(training_set, validation_set)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

validation_set_reduced <- perform_umap(validation_set)

get_stats(validation_set, prediction)
plot_predictions(validation_set_reduced, prediction, "Sprendimo medžio klasifikavimo rezultatai pilnai aibei")
roc_curve(validation_set, prediction_prob, positive_class = "2", "Sprendimo medžio ROC kreivė pilnai aibei")


# suspausta, atrinkta duomenu aibe
results <- apply_decision_tree(training_set_2d, validation_set_2d)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

get_stats(validation_set_2d, prediction)
plot_predictions(validation_set_2d, prediction, "Sprendimo medžio klasifikavimo rezultatai apribotai suspaustai aibei")
roc_curve(validation_set_2d, prediction_prob, positive_class = "2", "Sprendimo medžio ROC kreivė apribotai suspaustai aibei")