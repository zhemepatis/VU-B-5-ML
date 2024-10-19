target_cols <- c("RR_l_0", "seq_size", "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos", "label")

ekg_data <- ekg_data[, target_cols]
sample_0 <- sample_0[, target_cols]
sample_1 <- sample_1[, target_cols]
sample_2 <- sample_2[, target_cols]

# istrinam resursus, kuriu nebenaudosim
rm(target_cols)
