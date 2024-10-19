get_correlation_data <- function(data, target_cols, target_attr) {
  col_num <- length(target_cols)

  corr_data <- matrix(NA, ncol = col_num, nrow = col_num)
  colnames(corr_data) <- target_cols
  rownames(corr_data) <- target_cols

  for (feature1 in target_cols) {
    for (feature2 in target_cols) {
      test_result <- cor.test(data[[feature1]], data[[feature2]], method = "spearman", exact = FALSE)
      corr_data[feature1, feature2] <- test_result[[target_attr]]
    }
  }

  return(corr_data)
}

library(corrplot)

target_cols <- c("RR_l_0", "seq_size",  "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos", "label")

# gaunam koreliacijos koeficientus ir p reiksmes
corr_coef_table <- get_correlation_data(ekg_data, target_cols, "estimate")
p_value_table <- get_correlation_data(ekg_data, target_cols, "p.value")

# braizom koreliacijos grafika
corrplot(corr_coef_table, method = "circle", type = "lower", addCoef.col = "black")

# spausdinam koreliacijos koeficientu lentele bei p reiksmiu lentele
write.csv(corr_coef_table, "output/corr_coef_table.csv")
write.csv(p_value_table, "output/p_value_table.csv")

# istrinam resursus, kuriu nebenaudosim
rm(get_correlation_data)

rm(target_cols)
rm(corr_coef_table)
rm(p_value_table)