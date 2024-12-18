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
       xlab = "Medžių skaičius (ntree)", 
       ylab = "OOB klaidų dažnis",
       main = "Optimalus medžių skaičius",
       xlim = c(100, max_ntree), ylim = c(0, max(oob_errors) + 0.05))
  
  abline(v = optimal_ntree, col = "red", lwd = 2, lty = 2)
  
  return(list(optimal_ntree = optimal_ntree, optimal_oob_error = optimal_oob_error))
}


find_optimal_mtry_with_plot <- function(training_set, ntree = 500) {
  training_set$label <- as.factor(training_set$label)
  
  num_features <- ncol(training_set) - 1
  
  mtry_values <- 1:num_features
  
  oob_errors <- numeric(length(mtry_values))
  
  for (i in seq_along(mtry_values)) {
    mtry <- mtry_values[i]
    model <- randomForest(label ~ ., data = training_set, ntree = ntree, mtry = mtry)
    
    oob_errors[i] <- model$err.rate[ntree, "OOB"]
    
    print(sprintf("mtry = %d, OOB Error Rate = %.4f", mtry, oob_errors[i]))
  }
  
  optimal_mtry <- mtry_values[which.min(oob_errors)]
  optimal_oob_error <- min(oob_errors)
  
  print(sprintf("Optimal mtry: %d", optimal_mtry))
  print(sprintf("Corresponding OOB error rate: %.4f", optimal_oob_error))
  
  plot(mtry_values, oob_errors, type = "l", col = "blue", lwd = 2,
       xlab = "Svarstomų požymių skaičius (mtry)",
       ylab = "OOB klaidų dažnis",
       main = "Optimalus svarstomų požymių skaičius")
  abline(v = optimal_mtry, col = "red", lwd = 2, lty = 2)
  
  return(list(optimal_mtry = optimal_mtry, optimal_oob_error = optimal_oob_error))
}

