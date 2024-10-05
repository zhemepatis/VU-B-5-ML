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

# Išskirčių apribojimo funkcija (tik ekstremalioms)
cap_extreme_outliers <- function(x) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  extreme_H <- 3 * IQR(x, na.rm = TRUE)
  caps <- quantile(x, probs = c(.01, .99), na.rm = TRUE)
  
  x[x < (qnt[1] - extreme_H)] <- caps[1]
  x[x > (qnt[2] + extreme_H)] <- caps[2]
  
  return(x)
}

highlight_outliers <- function(data, col_name) {
  qnt <- quantile(data[[col_name]], probs = c(.25, .75), na.rm = TRUE)
  mild_H <- 1.5 * IQR(data[[col_name]], na.rm = TRUE)
  extreme_H <- 3 * IQR(data[[col_name]], na.rm = TRUE)
  
  # Finding mild and extreme outliers
  mild_outliers <- which((data[[col_name]] < (qnt[1] - mild_H) & data[[col_name]] >= (qnt[1] - extreme_H)) |
                           (data[[col_name]] > (qnt[2] + mild_H) & data[[col_name]] <= (qnt[2] + extreme_H)))
  extreme_outliers <- which(data[[col_name]] < (qnt[1] - extreme_H) | data[[col_name]] > (qnt[2] + extreme_H))
  
  return(list(mild = mild_outliers, extreme = extreme_outliers))
}


par(mfrow = c(1, 1), oma = c(0, 0, 4, 0))

# Sukuriamos lentelės išskirčių skaičiavimui
outlier_summary_before <- data.frame(Požymis = character(),
                                    Label = character(),
                                    Mild = integer(),
                                    Extreme = integer(),
                                    stringsAsFactors = FALSE)

outlier_summary_after <- data.frame(Požymis = character(),
                               Label = character(),
                               Mild = integer(),
                               Extreme = integer(),
                               stringsAsFactors = FALSE)

for (col in names(ekg_data)) {
  if (is.numeric(ekg_data[[col]])) {
    cat("Išskirtys apribojamos požymiui:", col, "\n")
    
    subset_0 <- ekg_data[ekg_data$label == 0,]
    subset_1 <- ekg_data[ekg_data$label == 1,]
    subset_2 <- ekg_data[ekg_data$label == 2,]
    
    outliers_0_before <- count_outliers(subset_0[[col]])
    outliers_1_before <- count_outliers(subset_1[[col]])
    outliers_2_before <- count_outliers(subset_2[[col]])
    
    outlier_summary_before <- rbind(outlier_summary_before, data.frame(Požymis = col, Label = "N(0)", Mild = outliers_0_before$mild, Extreme = outliers_0_before$extreme))
    outlier_summary_before <- rbind(outlier_summary_before, data.frame(Požymis = col, Label = "S(1)", Mild = outliers_1_before$mild, Extreme = outliers_1_before$extreme))
    outlier_summary_before <- rbind(outlier_summary_before, data.frame(Požymis = col, Label = "V(2)", Mild = outliers_2_before$mild, Extreme = outliers_2_before$extreme))
    
    outliers_0 <- highlight_outliers(subset_0, col)
    outliers_1 <- highlight_outliers(subset_1, col)
    outliers_2 <- highlight_outliers(subset_2, col)
    
    cat("Išskirčių skaičius prieš apribojimą požymiui", col, ":\n")
    cat("N(0) mild:", outliers_0_before$mild, ", extreme:", outliers_0_before$extreme, "\n")
    cat("S(1) mild:", outliers_1_before$mild, ", extreme:", outliers_1_before$extreme, "\n")
    cat("V(2) mild:", outliers_2_before$mild, ", extreme:", outliers_2_before$extreme, "\n")
    
    # Stačiakampės diagramos prieš apribojimą
    boxplot(subset_0[[col]], subset_1[[col]], subset_2[[col]],
            horizontal = TRUE, main = paste("Prieš išskirčių apribojimą"),
            names = c("N(0)", "S(1)", "V(2)"),
            cex.axis = 1.5, cex.lab = 1.5, cex.names = 1.5)
    mtext(paste("Stačiakampė diagrama požymio:", col), side = 3, line = 1.5, outer = TRUE, cex = 1.2, font = 2)
    
    points(subset_0[outliers_0$mild, col], rep(1, length(outliers_0$mild)), col = "orange", pch = 16, cex = 1.5)
    points(subset_1[outliers_1$mild, col], rep(2, length(outliers_1$mild)), col = "orange", pch = 16, cex = 1.5)
    points(subset_2[outliers_2$mild, col], rep(3, length(outliers_2$mild)), col = "orange", pch = 16, cex = 1.5)
    
    points(subset_0[outliers_0$extreme, col], rep(1, length(outliers_0$extreme)), col = "darkred", pch = 16, cex = 1.5)
    points(subset_1[outliers_1$extreme, col], rep(2, length(outliers_1$extreme)), col = "darkred", pch = 16, cex = 1.5)
    points(subset_2[outliers_2$extreme, col], rep(3, length(outliers_2$extreme)), col = "darkred", pch = 16, cex = 1.5)
    
    # Apribojamos tik ekstremalios išskirtys
    subset_0[[col]] <- cap_extreme_outliers(subset_0[[col]])
    subset_1[[col]] <- cap_extreme_outliers(subset_1[[col]])
    subset_2[[col]] <- cap_extreme_outliers(subset_2[[col]])
    
    ekg_data <- rbind(subset_0, subset_1, subset_2)
    
    # Išskirčių skaičius po apribojimo
    outliers_0_after <- count_outliers(subset_0[[col]])
    outliers_1_after <- count_outliers(subset_1[[col]])
    outliers_2_after <- count_outliers(subset_2[[col]])
    
    outliers_0 <- highlight_outliers(subset_0, col)
    outliers_1 <- highlight_outliers(subset_1, col)
    outliers_2 <- highlight_outliers(subset_2, col)
    
    cat("Išskirčių skaičius po apribojimo požymiui", col, ":\n")
    cat("N(0) mild:", outliers_0_after$mild, ", extreme:", outliers_0_after$extreme, "\n")
    cat("S(1) mild:", outliers_1_after$mild, ", extreme:", outliers_1_after$extreme, "\n")
    cat("V(2) mild:", outliers_2_after$mild, ", extreme:", outliers_2_after$extreme, "\n")
    
    # Stačiakampės diagramos po apribojimo
    boxplot(subset_0[[col]], subset_1[[col]], subset_2[[col]],
            horizontal = TRUE, main = paste("Po išskirčių apribojimo"),
            names = c("N(0)", "S(1)", "V(2)"),
            cex.axis = 1.5, cex.lab = 1.5, cex.names = 1.5)
    mtext(paste("Stačiakampė diagrama požymio:", col), side = 3, line = 1.5, outer = TRUE, cex = 1.2, font = 2)
    
    points(subset_0[outliers_0$mild, col], rep(1, length(outliers_0$mild)), col = "orange", pch = 16, cex = 1.5)
    points(subset_1[outliers_1$mild, col], rep(2, length(outliers_1$mild)), col = "orange", pch = 16, cex = 1.5)
    points(subset_2[outliers_2$mild, col], rep(3, length(outliers_2$mild)), col = "orange", pch = 16, cex = 1.5)
    
    points(subset_0[outliers_0$extreme, col], rep(1, length(outliers_0$extreme)), col = "darkred", pch = 16, cex = 1.5)
    points(subset_1[outliers_1$extreme, col], rep(2, length(outliers_1$extreme)), col = "darkred", pch = 16, cex = 1.5)
    points(subset_2[outliers_2$extreme, col], rep(3, length(outliers_2$extreme)), col = "darkred", pch = 16, cex = 1.5)
    
    # Pridedama į lentelę
    outlier_summary_after <- rbind(outlier_summary_after, data.frame(Požymis = col, Label = "N(0)", Mild = outliers_0_after$mild, Extreme = outliers_0_after$extreme))
    outlier_summary_after <- rbind(outlier_summary_after, data.frame(Požymis = col, Label = "S(1)", Mild = outliers_1_after$mild, Extreme = outliers_1_after$extreme))
    outlier_summary_after <- rbind(outlier_summary_after, data.frame(Požymis = col, Label = "V(2)", Mild = outliers_2_after$mild, Extreme = outliers_2_after$extreme))
    
  }
}

# Grafikų pateikimo laukas atstatomas
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

# Atstatoma objektų seka
rownames(ekg_data) <- 1:nrow(ekg_data)

# Išvedama lentelė su išskirčių informacija
cat("Išskirčių suvestinė prieš:\n")
print(outlier_summary_before)
cat("Išskirčių suvestinė po:\n")
print(outlier_summary_after)

# Statistiniai imties duomenys
sample_0 <- ekg_data[ekg_data$label == 0,]
sample_1 <- ekg_data[ekg_data$label == 1,]
sample_2 <- ekg_data[ekg_data$label == 2,]
