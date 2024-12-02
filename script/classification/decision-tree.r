library(rpart.plot)
library(rpart)
library(ggplot2)
source("script/analysis/prediction_plot.r")

# Ensure label columns are factors
training_set$label <- as.factor(training_set$label)
validation_set$label <- as.factor(validation_set$label)

# Train the decision tree model
model <- rpart(label ~ ., data = training_set)
rpart.plot(model, type = 1, extra = "auto")

# Make predictions on the validation set
target_cols <- setdiff(names(validation_set), "label")
prediction <- predict(model, newdata = validation_set[, target_cols], type = 'class')

# Confusion matrix
confusion_matrix <- table(validation_set$label, prediction)
print(confusion_matrix)

# Perform UMAP dimensionality reduction
training_set_reduced <- perform_umap(training_set)
validation_set_reduced <- perform_umap(validation_set)

# Add predictions and correctness information
validation_set_reduced$prediction <- prediction
validation_set_reduced$correct <- validation_set_reduced$label == prediction

plot_predictions(validation_set_reduced, prediction)