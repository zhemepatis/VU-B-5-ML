library("writexl")
library(caret)

source("script/analysis/prediction-stats.r")
source("script/classification/validation/validation-funcs.r")
source("script/classification/decision-tree.r")
source("script/data-preparation/norm.r")
source("script/dimension-reduction/umap.r")

apply_decision_tree_cross <- function(data, folds_num = 10, reduce = FALSE, cp = 0.0005, maxdepth = 15, minsplit = 20, minbucket = round(minsplit / 3)) {
  accuracy_intermediate <- numeric()
  micro_stats_intermediate_neg <- data.frame()
  micro_stats_intermediate_pos <- data.frame()
  macro_stats_intermediate <- data.frame()
  
  set.seed(1000)
  folds <- createFolds(data$label, folds_num, list = TRUE)
  
  for(idx in 1:folds_num) {
    validation_indices <- folds[[idx]]
    temp_validation_set <- data[validation_indices, ]

    training_indices <- setdiff(seq_len(nrow(data)), validation_indices)
    temp_training_set <- data[training_indices, ]
    
    normalization_result <- normalize_sets(temp_training_set, temp_validation_set)
    temp_training_set <- normalization_result$main_set
    temp_validation_set <- normalization_result$secondary_set

    if (reduce) {
      temp_training_set <- perform_umap(temp_training_set)
      temp_validation_set <- perform_umap(temp_validation_set)
    }
    
    # cia irgi passiname parametrus
    alg_results <- apply_decision_tree(temp_training_set, temp_validation_set, cp = cp, maxdepth = maxdepth, minsplit = minsplit, minbucket = minbucket)
    predictions <- alg_results$prediction

    confusion_matrix <- get_confusion_matrix(temp_validation_set, predictions)

    accuracy <- get_accuracy(confusion_matrix)
    accuracy_intermediate <- c(accuracy_intermediate, accuracy)
    
    micro_stats <- get_prediction_micro_stats(confusion_matrix)
    micro_stats_intermediate_neg <- rbind(micro_stats_intermediate_neg, micro_stats[micro_stats$label == 0,])
    micro_stats_intermediate_pos <- rbind(micro_stats_intermediate_pos, micro_stats[micro_stats$label == 2,])
    
    macro_stats <- get_prediction_macro_stats(confusion_matrix)
    macro_stats_intermediate <- rbind(macro_stats_intermediate, macro_stats)
  }
  
  result_stats <- sum_up_stats(accuracy_intermediate, micro_stats_intermediate_neg, micro_stats_intermediate_pos, macro_stats_intermediate)
  return(result_stats)
}

# nesuspausta, pilna duomenu aibe
cross_results <- apply_decision_tree_cross(training_set)

# suspausta, atrinkta duomenu aibe
cross_results_2d <- apply_decision_tree_cross(training_set_2d, reduce = TRUE)

cross_stats <- rbind(cross_results, cross_results_2d)
write_xlsx(cross_stats, "output/decision_tree_cross.xlsx")