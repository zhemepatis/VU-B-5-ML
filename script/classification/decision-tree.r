library(rpart.plot)
library(rpart)
library(ggplot2)
source("script/analysis/prediction_plot.r")

training_set$label <- as.factor(training_set$label)
validation_set$label <- as.factor(validation_set$label)

# apmokom modeli naudojant pilna duomenu aibe
model <- rpart(label ~ ., data = training_set)
rpart.plot(model, type = 1, extra = "auto")

# validuojam modeli
target_cols <- setdiff(names(validation_set), "label")
prediction <- predict(model, newdata = validation_set[, target_cols], type = 'class')

# skaiciuojam sumaisymo matrica
confusion_matrix <- table(validation_set$label, prediction)
print(confusion_matrix)

# sumazinam dimensijas vizualizavimui
training_set_reduced <- perform_umap(training_set)
validation_set_reduced <- perform_umap(validation_set)

# pridedam spejimus
validation_set_reduced$prediction <- prediction
validation_set_reduced$correct <- validation_set_reduced$label == prediction

# vizualizuojam rezultatus
plot_predictions(validation_set_reduced, prediction)