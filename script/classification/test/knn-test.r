library(rpart.plot)
library(rpart)

source("script/analysis/prediction-plot.r")
source("script/analysis/prediction-stats.r")
source("script/analysis/roc-curve.r")
source("script/classification/knn.r")
source("script/data-preparation/norm.r")
source("script/dimension-reduction/umap.r")

# normuojam duomenis
normalization_result <- normalize_sets(training_set, test_set)
training_set <- normalization_result$main_set
test_set <- normalization_result$secondary_set

target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")
training_set_2d <- training_set[, target_cols]
test_set_2d <- test_set[, target_cols]

# nesuspausta, pilna duomenu aibe
results <- apply_knn(training_set, test_set)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

test_set_reduced <- perform_umap(test_set, set_seed = TRUE)

plot_predictions(test_set_reduced, prediction, "kNN klasifikavimo rezultatai pilnai aibei")
auc <- roc_curve(test_set, prediction_prob, positive_class = "2", "kNN ROC kreivė pilnai aibei")
print(auc)

# apirbota, suspausta duomenu aibe
training_set_2d <- perform_umap(training_set_2d, set_seed = TRUE)
test_set_2d <- perform_umap(test_set_2d, set_seed = TRUE)

results <- apply_knn(training_set_2d, test_set_2d)
prediction <- results$prediction
prediction_prob <- results$prediction_prob

plot_predictions(test_set_2d, prediction, "kNN klasifikavimo rezultatai apribotai suspaustai aibei")
auc <- roc_curve(test_set_2d, prediction_prob, positive_class = "2", "kNN ROC kreivė apribotai suspaustai aibei")
print(auc)