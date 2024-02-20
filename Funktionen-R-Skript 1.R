#2)a)
deskriptive_stat <- function(daten) {
  
  # berechnet arithmetisches Mittel 
  mean_value <- mean(daten, na.rm=T)
  
  # berechnet Median
  median_value <- median(daten, na.rm=T)
  
  # berechnet Standartabweichung 
  sd_value <- sd(daten, na.rm=T)
  
  # berechnet Varianz
  var_value <- var(daten, na.rm=T)
  
  # berechnet Minimum und Maximum
  min_value <- min(daten, na.rm=T)
  max_value <- max(daten, na.rm=T)
  
  # berechnet Quantile
  quartiles <- quantile(daten, probs = c(0.25, 0.75), na.rm=T)
  
  # Gibt die Ergebnisse raus 
  cat("Mean:", mean_value, "\n")
  cat("Median:", median_value, "\n")
  cat("Standard Deviation:", sd_value, "\n")
  cat("Variance:", var_value, "\n")
  cat("Minimum:", min_value, "\n")
  cat("Maximum:", max_value, "\n")
  cat("1st Quartile:", quartiles[1], "\n")
  cat("3rd Quartile:", quartiles[2], "\n")
}
# Aufgabe 2 (ii)
# Empirische Entropie berechnen
entropieBerechnen <- function(relH) {
  freq <- relH / 100 
  k <- length(freq)
  entropie <- sum(freq * log(1/freq)) / log(k)
  return(entropie)
}

# Funktion zur Erstellung der Häufigkeitstabelle mit Modus und Entropie
Haeufigkeitstabelle <- function(katVar) {
  tab <- table(katVar)
  relH <- prop.table(tab) * 100
  ergebnis <- data.frame(
    AbsH = as.integer(tab),
    RelH = round(relH, 2),
    Modus = ifelse(tab == max(tab), "Ja", "Nein")
  )
  ent <- entropieBerechnen(relH)
  ergebnis$RelH.katVar <- NULL 
  print(ergebnis)
  cat("Empirische Entropie:", ent, "\n")
  return(list(Tab = ergebnis, Ent = ent))
}
#2iv)
#Maße fuer den Zusammenhang einer binären und einer 
#metrisch skalierten Variable
katbinom <- function(x,y, na.rm = FALSE){
  x <- as.numeric(x)
  y <- as.numeric(y)
  if(na.rm == TRUE){
    Tabelle <- data.frame(Pearson = cor(x,y,method = "pearson", use = "complete.obs"),
                          Kendall =  cor(x,y,method = "kendall", use = "complete.obs"),
                          Spearman = cor(x,y,method = "spearman", use = "complete.obs"))
    print(Tabelle)
  }
  else{
    Tabelle <- data.frame(Pearson = cor(x,y,method = "pearson", use = "all.obs"),
                          Kendall =  cor(x,y,method = "kendall", use = "all.obs"),
                          Spearman = cor(x,y,method = "spearman", use = "all.obs"))
    print(Tabelle)
  }
}

2(iii)
bivariate_stat <- function(variable1, variable2) {
  kontingenztabelle <- table(variable1, variable2)
  chiquadrat_test <- chisq.test(kontingenztabelle)
  
  cat("Kontingenztabelle:\n")
  print(kontingenztabelle)
  
  cat("\nChi-Quadrat-Test:\n")
  print(chiquadrat_test)
}

