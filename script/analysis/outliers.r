# Išskirčių skaičiavimo funkcija
count_outliers <- function(x) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  mild_H <- 1.5 * IQR(x, na.rm = TRUE)
  extreme_H <- 3 * IQR(x, na.rm = TRUE)
  
  mild_outliers <- sum((x < (qnt[1] - mild_H) & x >= (qnt[1] - extreme_H)) |
                         (x > (qnt[2] + mild_H) & x <= (qnt[2] + extreme_H)))
  
  extreme_outliers <- sum(x < (qnt[1] - extreme_H) | x > (qnt[2] + extreme_H))
  
  return(list(mild = mild_outliers, extreme = extreme_outliers))
}

# Išskirčių šalinimo funkcija
is_extreme_outlier <- function(x) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  extreme_H <- 3 * IQR(x, na.rm = TRUE)
  caps <- quantile(x, probs = c(.001, .999), na.rm = TRUE)
  
  (x < (qnt[1] - extreme_H) & x < caps[1]) | (x > (qnt[2] + extreme_H) & x > caps[2])
}

outlier_summary_before <- data.frame()
outlier_summary_after <- data.frame()

rows_to_remove <- rep(FALSE, nrow(ekg_data))

for (col in names(ekg_data)) {
  if (is.numeric(ekg_data[[col]])) {
    cat("Išskirtys apdorojamos požymiui:", col, "\n")
    
    boxplot(ekg_data[[col]], main = paste("Stačiakampė", col, "diagrama prieš išskirčių šalinimą"), col = "skyblue")
    
    outliers_before <- count_outliers(ekg_data[[col]])
    outlier_summary_before <- rbind(outlier_summary_before, data.frame(Požymis = col, Mild = outliers_before$mild, Extreme = outliers_before$extreme))
    
    rows_to_remove <- rows_to_remove | is_extreme_outlier(ekg_data[[col]])
    
    outliers_after <- count_outliers(ekg_data[[col]][!rows_to_remove])
    outlier_summary_after <- rbind(outlier_summary_after, data.frame(Požymis = col, Mild = outliers_after$mild, Extreme = outliers_after$extreme))
    
    boxplot(ekg_data[[col]][!rows_to_remove], main = paste("Stačiakampė", col, "diagrama po išskirčių šalinimą"), col = "lightgreen")
  }
}

ekg_data <- ekg_data[!rows_to_remove, ]

rownames(ekg_data) <- 1:nrow(ekg_data)


print(outlier_summary_before)
write.csv(outlier_summary_before, "output/outlier_summary_before.csv")

print(outlier_summary_after)
write.csv(outlier_summary_after, "output/outlier_summary_after.csv")
