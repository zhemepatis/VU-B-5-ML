get_correlation_table <- function(data, target_cols) {
  correlation_table <- matrix(NA, ncol=col_num, nrow=col_num)
  colnames(correlation_table) <- target_cols
  rownames(correlation_table) <- target_cols
  
  # skaiciuojam koreliacija kiekvieno stulpelio su kiekvienu stulpeliu
  for (attr1 in target_cols) {
    for (attr2 in target_cols) {
      correlation_table[attr1, attr2] <- cor(ekg_data[[attr1]], ekg_data[[attr2]], method="pearson")
    }
  }
  
  return(correlation_table)
}
