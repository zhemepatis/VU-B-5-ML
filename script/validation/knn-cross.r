library(caret)
source("script/analysis/prediction-stats.r")
source("script/classification/knn.r")
source("script/data-preparation/norm.r")
source("script/dimension-reduction/umap.r")

apply_knn_cross <- function(data, folds_num = 10, reduce = FALSE) {
  accuracy_intermediate <- numeric()
  micro_stats_intermediate <- data.frame()
  macro_stats_intermediate <- data.frame()
  
  folds <- createFolds(data$label, folds_num, list = TRUE)
  
  for(idx in 1:folds_num) {
    validation_indices <- folds[[idx]]
    temp_validation_set <- data[validation_indices, ]
    
    training_indices <- setdiff(seq_len(nrow(data)), validation_indices)
    temp_training_set <- data[training_indices, ]
    
    temp_training_set <- normalize_data(temp_training_set)
    temp_validation_set <- normalize_data(temp_validation_set)
    
    if (reduce) {
      temp_training_set <- perform_umap(temp_training_set)
      temp_validation_set <- perform_umap(temp_validation_set)
    }
    
    alg_results <- apply_knn(temp_training_set, temp_validation_set)
    predictions <- alg_results$prediction
    
    confusion_matrix <- get_confusion_matrix(temp_validation_set, predictions)
    accuracy <- get_accuracy(confusion_matrix)
    micro_stats <- get_prediction_micro_stats(confusion_matrix)
    macro_stats <- get_prediction_macro_stats(confusion_matrix)
    
    accuracy_intermediate <- c(accuracy_intermediate, accuracy)
    micro_stats_intermediate <- rbind(micro_stats_intermediate, micro_stats)
    macro_stats_intermediate <- rbind(macro_stats_intermediate, macro_stats)
  }
  
  accuracy_result <- data.frame(
    accuracy_mean = mean(accuracy_intermediate),
    accuracy_variance = var(accuracy_intermediate)
  )
  
  micro_stats_result <- data.frame(
    precision_mean = mean(micro_stats_intermediate$precision),
    precision_variance = var(micro_stats_intermediate$precision),
    recall_mean = mean(micro_stats_intermediate$recall),
    recall_variance = var(micro_stats_intermediate$recall),
    f1_mean = mean(micro_stats_intermediate$f1),
    f1_variance = var(micro_stats_intermediate$f1)
  )
  
  macro_stats_result <- data.frame(
    precision_mean = mean(macro_stats_intermediate$precision),
    precision_variance = var(macro_stats_intermediate$precision),
    recall_mean = mean(macro_stats_intermediate$recall),
    recall_variance = var(macro_stats_intermediate$recall),
    f1_mean = mean(macro_stats_intermediate$f1),
    f1_variance = var(macro_stats_intermediate$f1)
  )
  
  return(list(
    accuracy = accuracy_result,
    micro_stats = micro_stats_result,
    macro_stats = macro_stats_result
  ))
}

# nesuspausta, pilna duomenu aibe
cross_results <- apply_knn_cross(training_set)

# suspausta, atrinkta duomenu aibe
cross_results_2d <- apply_knn_cross(training_set_2d, reduce = TRUE)