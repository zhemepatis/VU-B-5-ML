library(rpart.plot)
library(rpart)
library(caret)

source("script/analysis/prediction-plot.r")
source("script/analysis/prediction-stats.r")
source("script/classification/logistic-regression.r")
source("script/data-preparation/norm.r")
source("script/dimension-reduction/umap.r")

# normuojam duomenis
normalization_result <- normalize_sets(training_set, test_set)
training_set <- normalization_result$main_set
test_set <- normalization_result$secondary_set

target_cols <- c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "P_pos", "wr_side", "label")
training_set_2d <- training_set[, target_cols]
lr_test_set <- test_set[, target_cols]


# nesuspausta, pilna duomenu aibe
lr_results <- apply_logistic_regression(training_set, test_set, threshold = 0.5, maxit = 1000, epsilon = 1e-8)
prediction <- lr_results$prediction
prediction <- factor(lr_results$prediction, levels = levels(as.factor(test_set$label)))
prediction_prob <- lr_results$prediction_prob

conf_matrix <- confusionMatrix(prediction, as.factor(test_set$label), positive = "2")
print(conf_matrix)
metrics <- compute_metrics(conf_matrix)

test_set_reduced <- perform_umap(test_set, set_seed = TRUE)

plot_predictions(test_set_reduced, prediction, "Logistinės regresijos klasifikavimo rezultatai pilnai aibei")


# apirbota, suspausta duomenu aibe
training_set_2d <- perform_umap(training_set_2d, set_seed = TRUE)
lr_test_set_2d <- perform_umap(test_set_2d, set_seed = TRUE)

lr_results_2d <- apply_logistic_regression(training_set_2d, test_set_2d, threshold = 0.5, maxit = 1000, epsilon = 1e-8)
prediction <- lr_results_2d$prediction
prediction <- factor(lr_results_2d$prediction, levels = levels(as.factor(test_set$label)))
prediction_prob <- lr_results_2d$prediction_prob

conf_matrix <- confusionMatrix(prediction, as.factor(test_set_2d$label), positive = "2")
print(conf_matrix)
metrics <- compute_metrics(conf_matrix)

plot_predictions(test_set_2d, prediction, "Logistinės regresijos klasifikavimo rezultatai apribotai suspaustai aibei")