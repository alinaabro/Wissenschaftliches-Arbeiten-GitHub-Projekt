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

#2(ii)
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

#2(iii)
bivariate_stat <- function(variable1, variable2) {
  kontingenztabelle <- table(variable1, variable2)
  chiquadrat_test <- chisq.test(kontingenztabelle)
  
  cat("Kontingenztabelle:\n")
  print(kontingenztabelle)
  
  cat("\nChi-Quadrat-Test:\n")
  print(chiquadrat_test)
}

#2(v) Erstellt einen Mosaikplot der gegebenen 3 Merkmale
#visualisierung(merkmal1, merkmal2, merkmal3, "NameMerkmal1", "NameMerkmal2", "NameMerkmal3")
visualisierung <- function(merkmal1, merkmal2, merkmal3, m1name=NA, m2name=NA, m3name=NA){
   mosaicplot(table(merkmal1,merkmal2,merkmal3), main=m3name, xlab=m1name, ylab=m2name)
}
  
# 2.vi

# Die Funktion plot_grouped_histogram erstellt ein Histogramm der Variable metrVar, das nach der Variable katVar gruppiert ist.
# Mit legend_names kann man die Legendenbeschriftungen setzen
# Zusätzliche Argumente können verwendet werden, um das grafische Layout anzupassen.
# Beispiel: plot_grouped_histogram("Age", "Sex", legend_names = c("Maennlich", "Weiblich"), main = "Alterverteilung nach Geschlecht")

plot_grouped_histogram <- function(metrVar, katVar, legend_names = NA, data = daten, ...) {
  # Konvertiere katVar in einen Faktor, falls es noch keiner ist
  data[[katVar]] <- factor(data[[katVar]])
  
  # Prüfe, ob katVar ein geordneter Faktor ist
  if(is.ordered(data[[katVar]])) {
    # Erhalte die eindeutigen Werte von katVar in der Reihenfolge der Faktorstufen
    categories <- levels(data[[katVar]])
  } else {
    # Erhalte die eindeutigen Werte von katVar
    categories <- unique(data[[katVar]])
  }
  
  # Setze Farben mit Transparenz für die Kategorien
  colors <- adjustcolor(rainbow(length(categories)), alpha = 0.5)
  
  # Bestimme die Intervallgrenzen für die Histogramme
  hist_data <- hist(data[[metrVar]], plot = FALSE)
  breaks <- hist_data$breaks
  
  # Erstelle eine Matrix, um die Histogrammzählungen für jede Kategorie zu speichern
  hist_counts <- matrix(NA, nrow = length(categories), ncol = length(breaks) - 1)
  
  # Schleife durch jede Kategorie und berechne die Histogrammzählungen
  for (i in 1:length(categories)) {
    # Check for NA category
    if (is.na(categories[i])) {
      # Subset data with NA values in katVar
      hist_counts[i, ] <- hist(data[is.na(data[[katVar]]),][[metrVar]], breaks = breaks, plot = FALSE)$counts
      categories[i] <- "Keine Daten"
    } else {
      # Subset data for current category
      hist_counts[i, ] <- hist(data[data[[katVar]] == categories[i],][[metrVar]], breaks = breaks, plot = FALSE)$counts
    }
  }
  
  # Plotte die Histogramme
  barplot(hist_counts, col = colors, beside = TRUE, names.arg = paste("(", round(breaks[-length(breaks)], 2), ",", round(breaks[-1], 2), "]", sep = ""), xlab = metrVar, ...)
  
  # Füge eine Legende hinzu
  if (sum(is.na(categories)) > 0) {
    categories <- as.vector(categories)
    categories[is.na(categories)] <- "Keine Daten"
  }
  if (length(legend_names) > 1) legend("topright", legend = legend_names, fill = colors)
  else legend("topright", legend = categories, fill = colors)
}

