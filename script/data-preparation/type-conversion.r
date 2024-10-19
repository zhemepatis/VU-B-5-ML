# neegzistuojanciu reiksmiu (NA) vertimas i skaicius ismeta ispejimus
# jiems ignoruoti yra naudojamas suppressWarnings(...)
# konvertuojant NA i skaiciu, as.numeric(...) palieka ta reiksme kaip NA
# sio reiksmes yra pasalinamos neegzistuojanciu reiksmiu valymo etape
ekg_data$RR_r_0 <- suppressWarnings(as.numeric(ekg_data$RR_r_0))
ekg_data$RR_r_0.RR_r_1 <- suppressWarnings(as.numeric(ekg_data$RR_r_0.RR_r_1))