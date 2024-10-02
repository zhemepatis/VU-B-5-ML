library(corrplot)
source("script/functions/correlation.r")

target_cols <- c("RR_l_0", "seq_size",  "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos")
col_num <- length(target_cols)

# koreliacija visiems duomenims bendrai
correlation_table <- get_correlation_table(ekg_data, target_cols)
write.csv(correlation_table, "correlation_table.csv")
corrplot(correlation_table, method="circle", type="lower", addCoef.col="black")