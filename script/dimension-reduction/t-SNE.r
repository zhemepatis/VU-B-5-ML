apply_and_plot_t_sne <- function (data, parameters, label_col, title) {
  # istraukiam parametrus
  perplexity <- as.numeric(parameters[[1]])
  pca_init <- as.logical(parameters[[2]])
  max_iter <- as.numeric(parameters[[3]])
  eta <- as.numeric(parameters[[4]])
  exaggeration_factor <- as.numeric(parameters[[5]])
  
  # pritaikom metoda su parinktais parametrais
  tsne_result <- Rtsne(
    data,
    perplexity = perplexity,
    pca = pca_init,
    max_iter = max_iter,
    eta = eta,
    exaggeration_factor = exaggeration_factor,
    normalize = FALSE
  )
  
  # issaugom tasku koordinates
  tsne_coords <- tsne_result$Y
  colnames(tsne_coords) <- c("dim_1", "dim_2")
  
  # pridedam klases prie duomenu
  tsne_df <- data.frame(tsne_coords, Label = as.factor(label_col))
  max_range <- max(abs(range(tsne_df$dim_1)), abs(range(tsne_df$dim_2)))
  
  # braizom grafika
  ggplot(tsne_df, aes(x = dim_1, y = dim_2, color = Label)) +
    geom_point(size = 2) +
    labs(title = title, 
         x = "t-SNE 1 dimensija",
         y = "t-SNE 2 dimensija",
         caption = paste0(
           "Parametrai: distance metric = ", "euclidean", 
           ", perplexity = ", perplexity, 
           ", PCA initialisation = ", pca_init,
           ", number of iterations = ", max_iter,
           ", exaggeration factor = ", exaggeration_factor,
           ", learning rate = ", eta
         )
    ) +
    scale_x_continuous(limits = c(-max_range, max_range)) +
    scale_y_continuous(limits = c(-max_range, max_range)) +
    coord_equal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.caption = element_text(hjust = 0.5, size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      axis.title = element_text(size = 14),
    )
}

library(Rtsne)
library(ggplot2)

set.seed(1000)

# pridedam duomenu normalizavimo koda
source("script/data-preparation/norm.r")

# atskiriam klasiu stulpeli
labels <- ekg_data[, 32]

# pritaikom t-SNE metoda nenormuotai visu pozymiu duomenu aibei
no_target_features <- ekg_data[, -32]

perplexity <- 30
pca_init <- TRUE
max_iter <- 1000
eta <- 200
exaggeration_factor <- 12
normalize <- FALSE
params <- list(perplexity, pca_init, max_iter, eta, exaggeration_factor)
apply_and_plot_t_sne(
  no_target_features,
  params, 
  labels,
  "t-SNE vizualizacija nenormuotai visų požymių duomenų aibei"
)

# pritaikom t-SNE metoda normuotai visu pozymiu duomenu aibei
no_target_features_normalised <- ekg_data_minmax[, -32]

perplexity <- 30
pca_init <- TRUE
max_iter <- 1000
eta <- 200
exaggeration_factor <- 12
normalize <- TRUE
params <- list(perplexity, pca_init, max_iter, eta, exaggeration_factor)
apply_and_plot_t_sne(
  no_target_features_normalised,
  params, 
  labels,
  "t-SNE vizualizacija normuotai visų požymių duomenų aibei"
)


# pritaikom t-SNE metoda normuotai atrinktai duomenu aibei
target_cols <- c("signal_mean", "signal_std", "T_pos","R_val", "Q_pos", "Q_val")
with_target_features <- ekg_data_minmax[target_cols]




