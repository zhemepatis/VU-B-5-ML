library(pROC)

# Source necessary scripts
source("script/analysis/prediction-plot.r")
source("script/analysis/prediction-stats.r")
source("script/classification/decision-tree.r")
source("script/classification/logistic-regression.r")
source("script/classification/random-forest.r")
source("script/data-preparation/norm.r")
source("script/dimension-reduction/umap.r")

# Normalize datasets
normalization_result <- normalize_sets(training_set, test_set)
training_set <- normalization_result$main_set
test_set <- normalization_result$secondary_set

target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")
training_set_2d <- training_set[, target_cols]
test_set_2d <- test_set[, target_cols]


# Logistic Regression ROC
lr_results <- apply_logistic_regression(training_set, test_set, threshold = 0.5, maxit = 1000, epsilon = 1e-8)
lr_roc <- roc(test_set$label, lr_results$prediction_prob[, "2"], levels = rev(levels(test_set$label)), plot = FALSE)

# Decision Tree ROC
dt_results <- apply_decision_tree(training_set, test_set, cp = 0.0005, maxdepth = 15, minsplit = 20, minbucket = 7)
dt_roc <- roc(test_set$label, dt_results$prediction_prob[, "2"], levels = rev(levels(test_set$label)), plot = FALSE)

# Random Forest ROC
rf_results <- apply_random_forest(training_set, test_set, ntree = 450, mtry = 2)
rf_roc <- roc(test_set$label, rf_results$prediction_prob[, "2"], levels = rev(levels(test_set$label)), plot = FALSE)

# Plot all ROC curves on the same graph
plot(rf_roc, col = "red", lwd = 2, main = "ROC kreivės pilnai duomenų aibei")
lines(lr_roc, col = "green", lwd = 2)
lines(dt_roc, col = "blue", lwd = 2)

# Add legend
legend("bottom", # Moves the legend to the top left of the plot
       legend = c(
         paste("Random Forest: AUC =", round(auc(rf_roc), 3)),
         paste("Logistic Regression: AUC =", round(auc(lr_roc), 3)),
         paste("Decision Tree: AUC =", round(auc(dt_roc), 3))
       ), 
       col = c("red", "green", "blue"), 
       lwd = 3, 
       cex = 0.8)



# Logistic Regression ROC
training_set_2d <- perform_umap(training_set_2d, set_seed = TRUE)
test_set_2d <- perform_umap(test_set_2d, set_seed = TRUE)

lr_results <- apply_logistic_regression(training_set_2d, test_set_2d, threshold = 0.5, maxit = 1000, epsilon = 1e-8)
lr_roc <- roc(test_set_2d$label, lr_results$prediction_prob[, "2"], levels = rev(levels(test_set_2d$label)), plot = FALSE)

# Decision Tree ROC
dt_results <- apply_decision_tree(training_set_2d, test_set_2d, cp = 0.0005, maxdepth = 15, minsplit = 20, minbucket = 7)
dt_roc <- roc(test_set_2d$label, dt_results$prediction_prob[, "2"], levels = rev(levels(test_set_2d$label)), plot = FALSE)

# Random Forest ROC
rf_results <- apply_random_forest(training_set_2d, test_set_2d, ntree = 450, mtry = 2)
rf_roc <- roc(test_set_2d$label, rf_results$prediction_prob[, "2"], levels = rev(levels(test_set_2d$label)), plot = FALSE)

# Plot all ROC curves on the same graph
plot(rf_roc, col = "red", lwd = 2, main = "ROC kreivės dvimatei duomenų aibei")
lines(lr_roc, col = "green", lwd = 2)
lines(dt_roc, col = "blue", lwd = 2)

# Add legend
legend("bottom", # Moves the legend to the top left of the plot
       legend = c(
         paste("Random Forest: AUC =", round(auc(rf_roc), 3)),
         paste("Logistic Regression: AUC =", round(auc(lr_roc), 3)),
         paste("Decision Tree: AUC =", round(auc(dt_roc), 3))
       ), 
       col = c("red", "green", "blue"), 
       lwd = 3, 
       cex = 0.8)

