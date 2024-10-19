sum_if_target_empty <- function(data, target_col, sum_col1, sum_col2) {
  for (i in seq_len(nrow(data))) {
    if (is.na(data[i, target_col])) {
      data[i, target_col] <- data[i, sum_col1] + data[i, sum_col2]
    }
  }

  return(data)
}

multiply_if_target_empty <- function(data, target_col, factor_col1, factor_col2) {
  for (i in seq_len(nrow(data))) {
    if (is.na(data[i, target_col])) {
      data[i, target_col] <- data[i, factor_col1] * data[i, factor_col2]
    }
  }

  return(data)
}

delete_if_target_empty <- function(data, target_col) {
  data <- data[!is.na(data[[target_col]]), ]
  return(data)
}

ekg_data <- multiply_if_target_empty(ekg_data, "RR_l_0", "RR_l_0.RR_l_1", "RR_l_1")
ekg_data <- sum_if_target_empty(ekg_data, "seq_size", "wl_side", "wr_side")

# istrinam resursus, kuriu nebenaudosim
rm(sum_if_target_empty)
rm(multiply_if_target_empty)
rm(delete_if_target_empty)
