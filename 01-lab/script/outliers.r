#
# Išskirtys, jų apribojimas
#
# Išskirtys esančios uz vidinių ar isorinių barjerų įgauna 5 ar 95 procentilės reikšmes,
# tinkančios reikšmės nėra keičiamos.

# Išskirčių skaičiavimo funkcija
count_outliers <- function(x) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  outliers <- sum(x < (qnt[1] - H) | x > (qnt[2] + H))
  return(outliers)
}

# Išskirčių apribojimo funkcija
cap_outliers <- function(x) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  caps <- quantile(x, probs = c(.05, .95 ), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  
  return(x)
}

par(mfrow = c(1, 1))  
boxplot(ekg_data, horizontal = TRUE)

# Grafikų pateikimo laukas padalinamas į dvi dalis
par(mfrow = c(1, 2), oma = c(0, 0, 4, 0))

for (col in names(ekg_data)) {
  if (is.numeric(ekg_data[[col]])) {
    cat("Išskirtys apribojamos požymiui:", col, "\n")
    
    subset_0 <- ekg_data[ekg_data$label == 0,]
    subset_1 <- ekg_data[ekg_data$label == 1,]
    subset_2 <- ekg_data[ekg_data$label == 2,]
    
    cat("Išskirčių skaičius prieš apribojimą požymiui", col, ":\n")
    cat("N(0) išskirtys:", count_outliers(subset_0[[col]]), "\n")
    cat("S(1) išskirtys:", count_outliers(subset_1[[col]]), "\n")
    cat("V(2) išskirtys:", count_outliers(subset_2[[col]]), "\n")
    
    boxplot(
      subset_0[[col]],
      subset_1[[col]],
      subset_2[[col]],
      horizontal = TRUE,
      main = paste("Prieš išskirčių apribojimą"),
      names = c("N(0)",
                "S(1)",
                "V(2)")
    )
    
    subset_0[[col]] <- cap_outliers(subset_0[[col]])
    subset_1[[col]] <- cap_outliers(subset_1[[col]])
    subset_2[[col]] <- cap_outliers(subset_2[[col]])
    
    ekg_data <- rbind(subset_0, subset_1, subset_2)
    
    cat("Išskirčių skaičius po apribojimo požymiui", col, ":\n")
    cat("N(0) išskirtys:", count_outliers(subset_0[[col]]), "\n")
    cat("S(1) išskirtys:", count_outliers(subset_1[[col]]), "\n")
    cat("V(2) išskirtys:", count_outliers(subset_2[[col]]), "\n")
    
    boxplot(
      subset_0[[col]],
      subset_1[[col]],
      subset_2[[col]],
      horizontal = TRUE,
      main = paste("Po išskirčių apribojimo"),
      names = c("N(0)",
                "S(1)",
                "V(2)")
    )
    
    mtext(paste("Išskirčių apribojimas:", col),
          side = 3,
          line = 1.5,
          outer = TRUE,
          cex = 1.2,
          font = 2)
  }
}

# Grafikų pateikimo laukas atstatomas
par(mfrow = c(1, 1))  

boxplot(ekg_data, horizontal = TRUE)


# Atstatoma objektu seka
rownames(ekg_data) <- 1:nrow(ekg_data)

# Statistiniai imties duomenys
subset_0 <- ekg_data[ekg_data$label == 0,]
subset_1 <- ekg_data[ekg_data$label == 1,]
subset_2 <- ekg_data[ekg_data$label == 2,]

cat("Imties: ")
summary(ekg_data)

cat("Label 0: ")
summary(subset_0)

cat("Label 1: ")
summary(subset_1)

cat("Label 2: ")
summary(subset_2)