library(ggplot2)

target_cols <- c("RR_l_0", "seq_size",  "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos")

for (i in 1:(length(target_cols) - 1)) {
  for (j in (i + 1):length(target_cols)) {
    col_x <- target_cols[i]
    col_y <- target_cols[j]
    
    plot <- ggplot(data = ekg_data, aes_string(x = col_x, y = col_y, colour = "factor(label)")) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
      labs(title = paste("Taškinė diagrama požymių", col_y, ":", col_x),
           x = col_x, y = col_y, colour = "Klasė") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12)
      )
    
    print(plot)
  }
}
