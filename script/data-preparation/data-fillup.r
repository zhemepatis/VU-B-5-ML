sum_if_target_empty <- function(data, target_col, sum_col1, sum_col2) {
  for (i in seq_len(nrow(data))) {
    if (is.na(data[i, target_col])) {
      data[i, target_col] <- data[i, sum_col1] + data[i, sum_col2]
    }
  }

  return(data)
}

subtract_if_target_empty <- function(data, target_col, minuend_col, subtrahend_col) {
  for (i in seq_len(nrow(data))) {
    if (is.na(data[i, target_col])) {
      data[i, target_col] <- data[i, minuend_col] + data[i, subtrahend_col]
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

divide_if_target_empty <- function(data, target_col, dividend_col, divisor_col) {
  for (i in seq_len(nrow(data))) {
    if (is.na(data[i, target_col])) {
      data[i, target_col] <- data[i, dividend_col] / data[i, divisor_col]
    }
  }

  return(data)
}

delete_if_target_empty <- function(data, target_col) {
  data <- data[!is.na(data[[target_col]]), ]
  return(data)
}

# uzpildom neegzistuojancias reiksmes isvestinem formulem
ekg_data <- sum_if_target_empty(ekg_data, "seq_size", "wl_side", "wr_side")
ekg_data <- subtract_if_target_empty(ekg_data, "wr_side", "seq_size", "wl_side")

ekg_data <- multiply_if_target_empty(ekg_data, "RR_l_0", "RR_l_0.RR_l_1", "RR_l_1")
ekg_data <- multiply_if_target_empty(ekg_data, "RR_l_2", "RR_l_2.RR_l_3", "RR_l_3")
ekg_data <- multiply_if_target_empty(ekg_data, "RR_r_0", "RR_r_0.RR_r_1", "RR_r_1")
ekg_data <- multiply_if_target_empty(ekg_data, "RR_r_1", "RR_r_1.RR_r_2", "RR_r_2")
ekg_data <- multiply_if_target_empty(ekg_data, "RR_r_2", "RR_r_2.RR_r_3", "RR_r_3")
ekg_data <- divide_if_target_empty(ekg_data, "RR_r_3", "RR_r_2", "RR_r_2.RR_r_3")

ekg_data <- divide_if_target_empty(ekg_data, "RR_l_2.RR_l_3", "RR_l_2", "RR_l_3")
ekg_data <- divide_if_target_empty(ekg_data, "RR_r_0.RR_r_1", "RR_r_0", "RR_r_1")
ekg_data <- divide_if_target_empty(ekg_data, "RR_r_1.RR_r_2", "RR_r_1", "RR_r_2")
ekg_data <- divide_if_target_empty(ekg_data, "RR_r_2.RR_r_3", "RR_r_2", "RR_r_3")

# istrinam eilutes, kuriose yra neegzistuojanciu reiksmiu,
# kuriu negalima uzpildyti formulem
target_cols <- colnames(ekg_data)
col_num <- length(target_cols)
for (idx in seq_len(col_num)) {
  curr_col <- target_cols[idx]
  ekg_data <- delete_if_target_empty(ekg_data, curr_col)
}

# istrinam resursus, kuriu nebenaudosim
rm(sum_if_target_empty)
rm(subtract_if_target_empty)
rm(multiply_if_target_empty)
rm(divide_if_target_empty)
rm(delete_if_target_empty)

rm(idx)
rm(col_num)
rm(target_cols)
rm(curr_col)