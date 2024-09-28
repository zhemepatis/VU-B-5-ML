# uzpildom RR_l_x / RR_l_x+1
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_l_0.RR_l_1", "RR_l_0", "RR_l_1")
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_l_1.RR_l_2", "RR_l_1", "RR_l_2")
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_l_2.RR_l_3", "RR_l_2", "RR_l_3")
# patikrinam ar dar yra praleistu reiksmiu
any(is.na(ekg_data$RR_l_0.RR_l_1)) 
any(is.na(ekg_data$RR_l_1.RR_l_2)) 
any(is.na(ekg_data$RR_l_2.RR_l_3))

# uzpildom RR_r_x / RR_r_x+1
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_r_0.RR_r_1", "RR_r_0", "RR_r_1")
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_r_1.RR_r_2", "RR_r_1", "RR_r_2")
ekg_data <- fill_in_missing_ratio(ekg_data, "RR_r_2.RR_r_3", "RR_r_2", "RR_r_3")
# patikrinam ar dar yra praleistu reiksmiu
any(is.na(ekg_data$RR_r_0.RR_r_1)) 
any(is.na(ekg_data$RR_r_1.RR_r_2)) 
any(is.na(ekg_data$RR_r_2.RR_r_3))

# sudedam uzpildytus duomenis i viena aibe
ekg_data <- rbind(sample_0, sample_1, sample_2)



any(complete.cases(ekg_data))