library(uwot)
library(ggplot2)
library(ggrepel)


# Min-max normavimo funkcija
min_max_normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x))) 
}

# Duomenu paruošimas, siekiant nepakeisti darbinės duomenų aibės
ekg_data_umap <- ekg_data

# Normuotos duomenų aibės kūrimas
ekg_data_umap_minmax <- ekg_data_umap
columns_to_normalize <- setdiff(names(ekg_data_umap), 'label')
for (col in columns_to_normalize) { 
  ekg_data_umap_minmax[[col]] <- min_max_normalize(ekg_data_umap_minmax[[col]]) 
}

# UMAP parametrai
set.seed(1000)
n_param <- 35
d_param <- 0.02
s_param <- 1.25
m_param <- 'euclidean'

# Funkcija apskaičiuoti UMAP ir pateikti grafiką
plot_umap <- function(data, title) {
  umap_result <- umap(
    data[, -which(names(data) == "label")], 
    n_neighbors = n_param, 
    min_dist = d_param, 
    spread = s_param,
    metric = m_param
  )
  
  umap_df <- data.frame(
    UMAP1 = umap_result[, 1],
    UMAP2 = umap_result[, 2],
    label = data$label
  )
  
  max_range <- max(abs(range(umap_df$UMAP1)), abs(range(umap_df$UMAP2)))
  
  ggplot(umap_df, aes(
    x = UMAP1, y = UMAP2, 
    col = as.factor(label)
  )) +
    geom_point() +
    scale_color_manual(values = c("0" = "red", "1" = "green", "2" = "blue")) +
    theme_minimal() +
    labs(
      color = "Klasė",
      title = title,
      x = "UMAP1",
      y = "UMAP2",
      caption = paste("UMAP parametrai: n_neighbours =", n_param, "; min_dist =", d_param, "; spread =", s_param, "; metric =", m_param)
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      plot.caption = element_text(size = 10, hjust = 0.5)
    ) +
    scale_x_continuous(limits = c(-max_range, max_range)) +
    scale_y_continuous(limits = c(-max_range, max_range)) +
    coord_equal()
}

# Duomenų aibės
datasets <- list(
  "nenormuota visų požymių duomenų aibė" = ekg_data_umap,
  "normuota visų požymių duomenų aibė" = ekg_data_umap_minmax,
  "normuota atrinktų požymių duomenų aibė" = ekg_data_umap_minmax[, c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "label")]
)

# Pateikiamas taškinis grafikas kiekvienos duomenų aibės
for (name in names(datasets)) {
  print(plot_umap(datasets[[name]], paste("UMAP vizualizacija:", name)))
}
