library("writexl")

source("script/analysis/prediction-stats.r")
source("script/classification/validation/validation-funcs.r")
source("script/classification/random-forest.r")
source("script/data-preparation/norm.r")
source("script/dimension-reduction/umap.r")

apply_random_forest_hold_out <- function(data, iteration_num = 10, reduce = FALSE) {
  accuracy_intermediate <- numeric()
  micro_stats_intermediate_neg <- data.frame()
  micro_stats_intermediate_pos <- data.frame()
  macro_stats_intermediate <- data.frame()
  
  for(idx in 1:iteration_num) {
    results <- split_data_into_sets(data)
    temp_training_set <- results$bigger_set
    temp_validation_set <- results$smaller_set
    
    normalization_result <- normalize_sets(temp_training_set, temp_validation_set)
    temp_training_set <- normalization_result$main_set
    temp_validation_set <- normalization_result$secondary_set
    
    if (reduce) {
      temp_training_set <- perform_umap(temp_training_set)
      temp_validation_set <- perform_umap(temp_validation_set)
    }
    
    alg_results <- apply_random_forest(temp_training_set, temp_validation_set)
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
  
  print(confusion_matrix)
  result_stats <- sum_up_stats(accuracy_intermediate, micro_stats_intermediate_neg, micro_stats_intermediate_pos, macro_stats_intermediate)
  return(result_stats)
}

# nesuspausta, pilna duomenu aibe
hold_out_results <- apply_random_forest_hold_out(training_set)

# suspausta, atrinkta duomenu aibe
hold_out_results_2d <- apply_random_forest_hold_out(training_set_2d, reduce = TRUE)

hold_out_stats <- rbind(hold_out_results, hold_out_results_2d)
write_xlsx(hold_out_stats, "output/random_forest_hold_out.xlsx")