source("script/functions/fillup.r")

# uzpildom duomenis isvestinem reiksmem
ekg_data <- multiply_if_target_empty(ekg_data, "RR_l_0", "RR_l_0.RR_l_1", "RR_l_1")
ekg_data <- sum_if_target_empty(ekg_data, "seq_size", "wl_side", "wr_side")

# pratrinam duomenu eilutes, kuriose yra duomenu, kuriu negalim uzpildyti isvestinem reiksmem
unfillable_cols = list("signal_mean", "signal_std", "R_val", "T_val", "R_pos", "T_pos", "label")
for (i in 1:length(unfillable_cols)) {
  curr_col_name = unfillable_cols[[i]]
  ekg_data <- delete_if_target_empty(ekg_data, curr_col_name)
}

# patikrinam, ar nera duomenu su neuzupildytom reiksmem
target_cols = append(unfillable_cols, "RR_l_0")
target_cols = append(unfillable_cols, "seq_size")
for (i in 1:length(target_cols)) {
  curr_col_name = target_cols[[i]]
  
  col_has_empty_values = has_empty_values(ekg_data, target_cols[[i]])
  if (col_has_empty_values) {
    print(curr_col_name + " has empty values")
  }
}