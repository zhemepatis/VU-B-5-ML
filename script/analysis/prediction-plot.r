library(ggplot2)

plot_predictions <- function(data, prediction, title = "Klasifikavimo rezultatai") {
  data$prediction <- prediction
  data$correct <- validation_set$label == prediction
  
  data$correct_label <- ifelse(data$correct, "Teisingas spėjimas", "Neteisingas spėjimas")
  
  ggplot(data, aes(x = V1, y = V2)) +
    geom_point(
      aes(
        fill = as.factor(prediction),
        color = correct_label
      ),
      size = 3,
      stroke = 1,
      shape = 21
    ) +
    scale_fill_manual(
      values = c("0" = "#52baff", "2" = "#fdae61"),
      name = "Klasės",
      labels = c("0" = "N(0) klasė", "2" = "V(2) klasė")
    ) +
    scale_color_manual(
      values = c("Teisingas spėjimas" = "black", "Neteisingas spėjimas" = "red"),
      name = "Spėjimo teisingumas"
    ) +
    labs(
      title = title,
      x = "UMAP1",
      y = "UMAP2"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
}
