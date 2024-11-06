library(ggplot2)

ekg_data_umap <- ekg_data
ekg_data_umap[, c("signal_mean", "signal_std", "R_val", "Q_pos", "Q_val", "T_pos", "label")]
umap_df <- perform_umap(ekg_data_umap)

max_range <- max(abs(range(umap_df$UMAP1)), abs(range(umap_df$UMAP2)))

ggplot(umap_df, aes(
  x = UMAP1, y = UMAP2
)) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Empirinis metodas",
    x = "UMAP1",
    y = "UMAP2"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  scale_x_continuous(limits = c(-max_range, max_range)) +
  scale_y_continuous(limits = c(-max_range, max_range)) +
  coord_equal()