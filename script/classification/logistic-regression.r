library(ggplot2)
library(pROC)
source("script/analysis/prediction-plot.r")
source("script/analysis/prediction-stats.r")
source("script/analysis/roc-curve.r")

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


# nesuspausta, pilna duomenu aibe
results <- apply_logistic_regression(training_set, validation_set)
prediction <- results$prediction
prediction_prob <- results$prediction_prob
validation_set_reduced <- perform_umap(validation_set)

get_stats(validation_set, prediction)
plot_predictions(validation_set_reduced, prediction, "Logistinės regresijos klasifikavimo rezultatai pilnai aibei")
roc_curve(validation_set, prediction_prob, positive_class = "2", "Logistinės regresijos ROC kreivė pilnai aibei")
get_auc(validation_set, prediction_prob, positive_class = "2")


# suspausta, atrinkta duomenu aibe
results <- apply_logistic_regression(training_set_2d, validation_set_2d)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

get_stats(validation_set_2d, prediction)
plot_predictions(validation_set_2d, prediction, "Logistinės regresijos klasifikavimo rezultatai apribotai suspaustai aibei")
roc_curve(validation_set_2d, prediction_prob, positive_class = "2", "Logistinės regresijos ROC kreivė apribotai suspaustai aibei")
get_auc(validation_set_2d, prediction_prob, positive_class = "2")
