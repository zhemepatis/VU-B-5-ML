summary_stats <- function(x) {
  min <- round(min(x, na.rm = TRUE), 4)
  max <- round(max(x, na.rm = TRUE), 4)
  q1 <- round(quantile(x, probs = 0.25, na.rm = TRUE), 4)
  q3 <- round(quantile(x, probs = 0.75, na.rm = TRUE), 4)
  mean <- round(mean(x, na.rm = TRUE), 4)
  median <- round(median(x, na.rm = TRUE), 4)
  std_dev <- round(sd(x, na.rm = TRUE), 4)
  na_count <- sum(is.na(x))

  return(c(Min = min, Q1 = q1, Median = median, Mean = mean, Q3 = q3, Max = max, Std_Dev = std_dev, NA_Count = na_count))
}

get_summary_table <- function(data, target_cols) {
  table <- data.frame(t(sapply(data[target_cols], summary_stats)))
  table <- data.frame(Statistic = row.names(table), table, row.names = NULL)

  return(table)
}

sample_0 <- ekg_data[ekg_data$label == 0,]
sample_2 <- ekg_data[ekg_data$label == 2,]

# atspausdinam aprasomasias statistikas
print("N(0) klasės aprasomoji statistika")
print(summary(sample_0))
print("V(2) klasės aprasomoji statistika")
print(summary(sample_2))

# surasom aprasomiasas statistikas i .csv failus
target_cols <- colnames(ekg_data)

sample_0_summary <- get_summary_table(sample_0, target_cols)
sample_2_summary <- get_summary_table(sample_2, target_cols)
# ekg_data_summary <- get_summary_table(ekg_data, target_cols)

write.csv(sample_0_summary, file = "output/sample_0_summary.csv", row.names = FALSE)
write.csv(sample_2_summary, file = "output/sample_2_summary.csv", row.names = FALSE)
# write.csv(ekg_data_summary, file = "output/ekg_data_summary.csv", row.names = FALSE)

# istrinam resursus, kuriu nebenaudosim
rm(summary_stats)
rm(get_summary_table)

rm(sample_0_summary)
rm(sample_2_summary)
# rm(ekg_data_summary)