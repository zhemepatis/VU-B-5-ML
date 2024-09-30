# Duomenu normavimas

min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

mean_std_normalize <- function(x) {
  return((x - mean(x)) / sqrt(var(x)))
}

ekg_data_minmax <- ekg_data
ekg_data_meanstd <- ekg_data

columns_to_normalize <- c("RR_l_0", "seq_size",  "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos")

for (col in columns_to_normalize) {
  ekg_data_minmax[[col]] <- min_max_normalize(ekg_data_minmax[[col]])
}

for (col in columns_to_normalize) {
  ekg_data_meanstd[[col]] <- mean_std_normalize(ekg_data_meanstd[[col]])
}


#
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

for (col in names(ekg_data_minmax)) {
  if (is.numeric(ekg_data_minmax[[col]])) {
    hist(ekg_data_minmax[[col]], main = paste("Histograma požymio:", col), xlab = col)
  }
}

for (col in names(ekg_data_meanstd)) {
  if (is.numeric(ekg_data_meanstd[[col]])) {
    hist(ekg_data_meanstd[[col]], main = paste("Histograma požymio:", col), xlab = col)
  }
}
