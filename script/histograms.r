target_cols <- c("RR_l_0", "seq_size",  "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos")
target_col_num <- length(target_cols)

# 0 klasės histogramos
for (i in 1:target_col_num) {
  curr_col_name <- target_cols[[i]]
  hist(sample_0[[curr_col_name]], main = paste0("N(0) klasės „", curr_col_name, "“ požymio histograma"), ylab = "Dažnis", xlab="Įgyjamų reikšmių intervalai")
}

# 1 klasės histogramos
for (i in 1:target_col_num) {
  curr_col_name <- target_cols[[i]]
  hist(sample_1[[curr_col_name]], main = paste0("S(1) klasės „", curr_col_name, "“ požymio histograma"), ylab = "Dažnis", xlab="Įgyjamų reikšmių intervalai")
}

# 2 klasės histogramos
for (i in 1:target_col_num) {
  curr_col_name <- target_cols[[i]]
  hist(sample_2[[curr_col_name]], main = paste0("V(2) klasės „", curr_col_name, "“ požymio histograma"), ylab = "Dažnis", xlab="Įgyjamų reikšmių intervalai")
}

# histogramos bendrai
for (i in 1:target_col_num) {
  curr_col_name <- target_cols[[i]]
  hist(ekg_data[[curr_col_name]], main = paste0("Visos duomenų aibės „", curr_col_name, "“ požymio histograma"), ylab = "Dažnis", xlab="Įgyjamų reikšmių intervalai")
}