get_histogram_title <- function(data_class, col) {
  return(paste0(data_class, " „", col, "“ požymio histograma"))
}

plot_histograms <- function(data, data_class, target_cols) {
  col_num <- length(target_cols)

  for (idx in seq_len(col_num)) {
    curr_col <- target_cols[idx]

    title <- get_histogram_title(data_class, curr_col)
    hist(data[[curr_col]], main = title, xlab = "Įgyjamų reikšmių intervalai", ylab = "Dažnis")
  }
}

target_cols <- c("RR_l_0", "seq_size",  "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos")
plot_histograms(sample_0, "N(0) klasės", target_cols)
plot_histograms(sample_0, "S(1) klasės", target_cols)
plot_histograms(sample_0, "V(2) klasės", target_cols)
plot_histograms(sample_0, "Visos duomenų aibės", target_cols)

# istrinam resursus, kuriu nebenaudosim
rm(get_histogram_title)
rm(plot_histograms)

rm(target_cols)