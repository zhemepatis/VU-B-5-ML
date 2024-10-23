library(Rtsne)
library(ggplot2)

features <- ekg_data[, -32] # paimam visus pozymius, isskyrus klase
labels <- ekg_data[, 32] # paimam klases stulpeli

# pritaikom t-SNE metoda
# TODO: when to turn off early exaggaration
tsne_result <- Rtsne(features, normalize_input = FALSE, perplexity = 5, max_iter = 1000)

# issaugom tasku koordinates
tsne_coords <- tsne_result$Y
colnames(tsne_coords) <- c("dim_1", "dim_2")

# pridedam klases prie duomenu
tsne_df <- data.frame(tsne_coords, Label = as.factor(labels))

# braizom grafika
ggplot(tsne_df, aes(x = dim_1, y = dim_2, color = Label)) +
  geom_point(size = 2) +
  labs(title = "t-SNE metodo vizualizavimas", x = "t-SNE 1 dimensija", y = "t-SNE 2 dimensija")