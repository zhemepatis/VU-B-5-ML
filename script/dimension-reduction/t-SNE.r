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
perplexity <- 40
max_iter <- 1000
exaggeration_factor <- 20
tsne_result <- Rtsne(features, normalize_input = FALSE, perplexity = perplexity, max_iter = max_iter, exaggeration_factor = exaggeration_factor)

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


# isankstines isvados

# didinant max_iter matosi, kad taskiukai spaudzias biskiuka viens prie kito
# taciau kazkokiu labai dideliu pokyciu nepastebima
# galima palikti 1000, kad algoritmas greiciau veiktu

# didinant perplexity akivaizdziai matosi skirtumas
# taskiukai kiek labiau susispaude, aiskiau matosi skirtingos klasiu sritys
# taciau vis tiek yra kazkokiu persypinimu
# nlb aisku kuris yra geriausias

# didinant exaggeration factor, matom, kad vienos klases debeseliai arteja vienas prie kito
# esant per dideliam EF, matom, kad debeseliai vel atsiskyre