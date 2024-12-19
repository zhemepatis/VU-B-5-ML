library(rpart.plot)
library(rpart)

source("script/analysis/prediction-plot.r")
source("script/analysis/prediction-stats.r")
source("script/classification/decision-tree.r")
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
dt_results <- apply_decision_tree(training_set, test_set)
prediction <- dt_results$prediction
prediction_prob <- dt_results$prediction_prob

test_set_reduced <- perform_umap(test_set, set_seed = TRUE)

plot_predictions(test_set_reduced, prediction, "Decision Tree klasifikavimo rezultatai pilnai aibei")


# apirbota, suspausta duomenu aibe
training_set_2d <- perform_umap(training_set_2d, set_seed = TRUE)
test_set_2d <- perform_umap(test_set_2d, set_seed = TRUE)

dt_results_2d <- apply_decision_tree(training_set_2d, test_set_2d)
prediction <- dt_results_2d$prediction
prediction_prob <- dt_results_2d$prediction_prob

plot_predictions(test_set_2d, prediction, "Decision Tree klasifikavimo rezultatai apribotai suspaustai aibei")