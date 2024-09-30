library(corrplot)

target_cols <- c("RR_l_0", "seq_size",  "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos")
col_num <- length(target_cols)

correlation_table <- matrix(NA, ncol=col_num, nrow=col_num)
colnames(correlation_table) <- target_cols
rownames(correlation_table) <- target_cols

# skaiciuojam koreliacija kiekvieno stulpelio su kiekvienu stulpeliu
for (attr1 in target_cols) {
  for (attr2 in target_cols) {
    correlation_table[attr1, attr2] <- cor(ekg_data[[attr1]], ekg_data[[attr2]], method="pearson")
  }
}

write.csv(correlation_table, "correlation_table.csv") # spausdinam koreliacijos lentelę
corrplot(correlation_table, method="circle", type="lower", addCoef.col="black") # piesiam koreliacijos diagrama
