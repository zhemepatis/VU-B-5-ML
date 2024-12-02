library(rpart.plot)
library(rpart)
library(ggplot2)
source("script/analysis/prediction_plot.r")
source("script/analysis/prediction-stats.r")

apply_decision_tree <- function(training_set, validation_set) {
  training_set$label <- as.factor(training_set$label)
  validation_set$label <- as.factor(validation_set$label)
  
  # apmokom modeli naudojant pilna duomenu aibe
  model <- rpart(label ~ ., data = training_set)
  rpart.plot(model, type = 1, extra = "auto")
  
  # validuojam modeli
  target_cols <- setdiff(names(validation_set), "label")
  prediction <- predict(model, newdata = validation_set[, target_cols], type = 'class')
  
  return(prediction)
}

# nesuspausta, pilna duomenu aibe
prediction <- apply_decision_tree(training_set, validation_set)
validation_set_reduced <- perform_umap(validation_set)
get_stats(validation_set, prediction)
plot_predictions(validation_set_reduced, prediction)

# suspausta, atrinkta duomenu aibe
prediction <- apply_decision_tree(training_set_2d, validation_set_2d)
get_stats(validation_set_2d, prediction)
plot_predictions(validation_set_2d, prediction)