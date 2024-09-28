summary(sample_0)
summary(sample_1)
summary(sample_2)

sample_binded <- rbind(sample_0, sample_1, sample_2)
summary(sample_binded)


columns <- c("RR_l_0", "seq_size", "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos", "label")
# 
# summary_stats <- function(x) {
#   Min <- round(min(x, na.rm = TRUE), 4)
#   Max <- round(max(x, na.rm = TRUE), 4)
#   Q1 <- round(quantile(x, probs = 0.25, na.rm = TRUE), 4)
#   Q3 <- round(quantile(x, probs = 0.75, na.rm = TRUE), 4)
#   Mean <- round(mean(x, na.rm = TRUE), 4)
#   Median <- round(median(x, na.rm = TRUE), 4)
#   Std_Dev <- round(sd(x, na.rm = TRUE), 4)
#   NA_Count <- sum(is.na(x))
#   
#   return(c(Min = Min, Q1 = Q1, Median = Median, Mean = Mean, Q3 = Q3, Max = Max, Std_Dev = Std_Dev, NA_Count = NA_Count))
# }
# 
# 
# summary_binded_table <- data.frame(t(sapply(sample_binded[columns], summary_stats)))
# summary_binded_table <- data.frame(Statistic = row.names(summary_binded_table), summary_binded_table, row.names = NULL)
# print(summary_binded_table)
# write.csv(summary_binded_table, file = "summary_binded_table.csv", row.names = FALSE)
# 
# summary_0_table <- data.frame(t(sapply(sample_0[columns], summary_stats)))
# summary_0_table <- data.frame(Statistic = row.names(summary_0_table), summary_0_table, row.names = NULL)
# print(summary_0_table)
# write.csv(summary_0_table, file = "summary_0_table.csv", row.names = FALSE)
# 
# summary_1_table <- data.frame(t(sapply(sample_1[columns], summary_stats)))
# summary_1_table <- data.frame(Statistic = row.names(summary_1_table), summary_1_table, row.names = NULL)
# print(summary_1_table)
# write.csv(summary_1_table, file = "summary_1_table.csv", row.names = FALSE)
# 
# summary_2_table <- data.frame(t(sapply(sample_2[columns], summary_stats)))
# summary_2_table <- data.frame(Statistic = row.names(summary_2_table), summary_2_table, row.names = NULL)
# print(summary_2_table)
# write.csv(summary_2_table, file = "summary_2_table.csv", row.names = FALSE)