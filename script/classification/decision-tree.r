library(rpart.plot)
library(rpart)

# 
apply_decision_tree <- function(training_set, validation_set, plot_decision_tree = FALSE, cp = 0.01, maxdepth = 30, minsplit = 20, minbucket = round(minsplit / 3)) {
  training_set$label <- as.factor(training_set$label)
  validation_set$label <- as.factor(validation_set$label)
  
  # apmokom modeli naudojant pilna duomenu aibe
  
  # cia greiciausiai parametrai
  model <- rpart(label ~ ., data = training_set, 
                 control = rpart.control(cp = cp, maxdepth = maxdepth, 
                                         minsplit = minsplit, minbucket = minbucket))
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

