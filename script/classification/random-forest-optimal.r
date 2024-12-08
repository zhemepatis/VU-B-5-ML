find_optimal_ntree_with_plot <- function(training_set, max_ntree = 1000, step = 10) {
  training_set$label <- as.factor(training_set$label)
  
  oob_errors <- numeric(0)
  ntree_values <- numeric(0)
  
  for (ntree_value in seq(10, max_ntree, by = step)) {
    model <- randomForest(label ~ ., data = training_set, ntree = ntree_value)
    
    oob_errors <- c(oob_errors, model$err.rate[ntree_value, "OOB"])
    ntree_values <- c(ntree_values, ntree_value)
    
    print(paste("ntree =", ntree_value, "OOB Error Rate =", round(model$err.rate[ntree_value, "OOB"], 4)))
  }
  
  optimal_ntree <- ntree_values[which.min(oob_errors)]
  optimal_oob_error <- min(oob_errors)
  
  print(paste("Optimal ntree:", optimal_ntree))
  print(paste("Corresponding OOB error rate:", round(optimal_oob_error, 4)))
  
  plot(ntree_values, oob_errors, type = "l", col = "blue", lwd = 2,
       xlab = "Number of Trees (ntree)", ylab = "OOB Error Rate",
       main = "Optimalus metdžių skaičius",
       xlim = c(100, max_ntree), ylim = c(0, max(oob_errors) + 0.05))
  
  abline(v = optimal_ntree, col = "red", lwd = 2, lty = 2)
  
  return(list(optimal_ntree = optimal_ntree, optimal_oob_error = optimal_oob_error))
}
