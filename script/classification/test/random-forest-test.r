library(rpart.plot)
library(rpart)

source("script/analysis/prediction-plot.r")
source("script/analysis/prediction-stats.r")
source("script/analysis/roc-curve.r")
source("script/classification/random-forest.r")
source("script/data-preparation/norm.r")
source("script/dimension-reduction/umap.r")

# normuojam duomenis
normalization_result <- normalize_sets(training_set, test_set)
training_set <- normalization_result$main_set
test_set <- normalization_result$secondary_set

target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")
training_set_2d <- training_set[, target_cols]
test_set_2d <- test_set[, target_cols]


# optimalaus medziu skaiciaus pasirinkimas nesuspaustai pilnai duomenu aibei
results_optimal <- find_optimal_ntree_with_plot(training_set, max_ntree = 1000, step = 10)
optimal_ntree <- results_optimal$optimal_ntree
optimal_oob_error <- results_optimal$optimal_oob_error


# nesuspausta, pilna duomenu aibe
results <- apply_random_forest(training_set, test_set, ntree=180)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

test_set_reduced <- perform_umap(test_set, set_seed = TRUE)

plot_predictions(test_set_reduced, prediction, "Random Forest klasifikavimo rezultatai pilnai aibei")
auc <- roc_curve(test_set, prediction_prob, positive_class = "2", "Random Forest ROC kreivė pilnai aibei")
print(auc)

# apirbota, suspausta duomenu aibe
training_set_2d <- perform_umap(training_set_2d, set_seed = TRUE)
test_set_2d <- perform_umap(test_set_2d, set_seed = TRUE)


# optimalaus medziu skaiciaus pasirinkimas apribotai suspaustai duomenu aibei
results_optimal <- find_optimal_ntree_with_plot(training_set_2d, max_ntree = 1000, step = 10)
optimal_ntree <- results_optimal$optimal_ntree
optimal_oob_error <- results_optimal$optimal_oob_error


results <- apply_random_forest(training_set_2d, test_set_2d, ntree = 70)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

plot_predictions(test_set_2d, prediction, "Random Forest klasifikavimo rezultatai apribotai suspaustai aibei")
auc <- roc_curve(test_set_2d, prediction_prob, positive_class = "2", "Random Forest ROC kreivė apribotai suspaustai aibei")
print(auc)
