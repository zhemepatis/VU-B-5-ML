get_stats <- function(validation_set, prediction) {
  confusion_matrix <- table(validation_set$label, prediction)
  print("Confusion matrix:")
  print(confusion_matrix)
  
  prediction_micro_stats <- get_prediction_micro_stats(confusion_matrix)
  print("Micro statistics:")
  print(prediction_micro_stats)
  
  prediction_macro_stats <- get_prediction_macro_stats(confusion_matrix)
  print("Macro statistics:")
  print(prediction_macro_stats)
}

get_prediction_macro_stats <- function(confusion_matrix) {
  micro_stats <- get_prediction_micro_stats(confusion_matrix)

  precision <- mean(micro_stats$precision)
  recall <- mean(micro_stats$recall)
  f1 <- mean(micro_stats$f1)
  
  print(precision)
  
  result <- data.frame(
    precision,
    recall,
    f1
  )
  
  return(result)
}

get_prediction_micro_stats <- function(confusion_matrix) {
  precision <- get_precision(confusion_matrix)
  recall <- get_recall(confusion_matrix)
  f1 <- get_f1(confusion_matrix)
  
  result <- data.frame(
    precision,
    recall,
    f1
  )
  
  return(result)
}

get_accuracy <- function(confusion_matrix) {
  diagonal <- diag(confusion_matrix)
  instance_num <- get_instance_num(confusion_matrix)
  
  return(diagonal / instance_num)
}

get_instance_num <- function(confusion_matrix) {
  return(sum(confusion_matrix))
}

get_precision <- function(confusion_matrix) {
  diagonal <- diag(confusion_matrix)
  colsums <- apply(confusion_matrix, 1, sum)
  
  return(diagonal / colsums)
}

get_recall <- function(confusion_matrix) {
  diagonal <- diag(confusion_matrix)
  rowsums <- apply(confusion_matrix, 2, sum)
  
  return(diagonal / rowsums)
}

get_f1 <- function(confusion_matrix) {
  precision <- get_precision(confusion_matrix)
  recall <- get_recall(confusion_matrix)
  
  return(2 * precision * recall / (precision + recall))
}