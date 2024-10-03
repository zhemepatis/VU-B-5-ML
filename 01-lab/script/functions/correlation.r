get_correlation_coef_table <- function(data, target_cols) {
  col_num <- length(target_cols)
  
  coef_table <- matrix(NA, ncol=col_num, nrow=col_num)
  colnames(coef_table) <- target_cols
  rownames(coef_table) <- target_cols
  
  # skaiciuojam koreliacijos koeficienta kiekvieno stulpelio su kiekvienu stulpeliu
  for (attr1 in target_cols) {
    for (attr2 in target_cols) {
      result <- cor.test(ekg_data[[attr1]], ekg_data[[attr2]], method="spearman", exact=FALSE)
      coef_table[attr1, attr2] <- result$estimate
    }
  }
  
  return(coef_table)
}

get_correlation_p_value_table <- function(data, target_cols) {
  col_num <- length(target_cols)
  
  p_value_table <- matrix(NA, ncol=col_num, nrow=col_num)
  colnames(p_value_table) <- target_cols
  rownames(p_value_table) <- target_cols
  
  # skaiciuojam koreliacija p reiksme stulpelio su kiekvienu stulpeliu
  for (attr1 in target_cols) {
    for (attr2 in target_cols) {
      result <- cor.test(ekg_data[[attr1]], ekg_data[[attr2]], method="spearman", exact=FALSE)
      p_value_table[attr1, attr2] <- result$p.value
    }
  }
  
  return(p_value_table)
}

