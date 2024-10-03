library(corrplot)
source("script/functions/correlation.r")

target_cols <- c("RR_l_0", "seq_size",  "signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos", "label")

# koreliacijos koeficientas visiems duomenims bendrai
corr_coef_table <- get_correlation_coef_table(ekg_data, target_cols)
write.csv(corr_coef_table, "corr_coef_table.csv")
corrplot(corr_coef_table, method="circle", type="lower", addCoef.col="black")

p_value_table <- get_correlation_p_value_table(ekg_data, target_cols)
write.csv(p_value_table, "p_value_table.csv")