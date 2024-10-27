library(Rtsne)
library(ggplot2)

set.seed(1000)

# pridedam duomenu normalizavimo koda
source("script/data-preparation/norm.r")

target_cols <- c("signal_mean", "signal_std", "T_pos","R_val", "Q_pos", "Q_val")
features <- ekg_data_minmax[target_cols]
# features <- ekg_data[, -32]
labels <- ekg_data[, 32] # paimam klases stulpeli

# pritaikom t-SNE metoda
perplexity <- 40 # default 30
pca_init <- TRUE # default TRUE
max_iter <- 1000 # default 1000
eta <- 400
exaggeration_factor <- 20 # default 12

tsne_result <- Rtsne(
  features,
  perplexity = perplexity,
  pca = pca_init,
  max_iter = max_iter,
  eta = eta,
  exaggeration_factor = exaggeration_factor
)

# issaugom tasku koordinates
tsne_coords <- tsne_result$Y
colnames(tsne_coords) <- c("dim_1", "dim_2")

# pridedam klases prie duomenu
tsne_df <- data.frame(tsne_coords, Label = as.factor(labels))
max_range <- max(abs(range(tsne_df$dim_1)), abs(range(tsne_df$dim_2)))

# braizom grafika
ggplot(tsne_df, aes(x = dim_1, y = dim_2, color = Label)) +
  geom_point(size = 2) +
  labs(title = paste0("t-SNE metodo vizualizavimas (prplx=", perplexity, ", max_iter=", max_iter, ", exg_f=", exaggeration_factor, ")"), 
        x = "t-SNE 1 dimensija",
        y = "t-SNE 2 dimensija") +
  scale_x_continuous(limits = c(-max_range, max_range)) +
  scale_y_continuous(limits = c(-max_range, max_range)) +
  coord_equal()