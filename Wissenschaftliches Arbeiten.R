rm(list=ls())
#1)a) 
#Hier wurden die Anreden extrahiert und sortiert 
daten <- read.csv("/Users/hasanabd-alkareem/Downloads/titanic")
daten$Anrede<-gsub(".*, (\\w+)\\..*", "\\1", daten$Name)
daten$Anrede <- gsub("Miss", "Ms", daten$Anrede)
daten$Anrede <- gsub("Mlle", "Ms", daten$Anrede)
daten$Anrede <- gsub("Lady", "Mrs", daten$Anrede)
daten$Anrede[daten$Name == "Leader, Dr. Alice (Farnham)"] <- "Mrs"
daten$Anrede <- gsub("Dr", "Mr", daten$Anrede)
daten$Anrede <- gsub("Mme", "Mrs", daten$Anrede)
daten$Anrede<- gsub("Capt", "Mr", daten$Anrede)
daten$Anrede<- gsub("Col", "Mr", daten$Anrede)
daten$Anrede<- gsub("Don", "Mr", daten$Anrede)
daten$Anrede<- gsub("Jonkheer", "Mr", daten$Anrede)
daten$Anrede<- gsub("Rev", "Mr", daten$Anrede)
daten$Anrede<- gsub("Sir", "Mr", daten$Anrede)
daten$Anrede<- gsub("Major", "Mr", daten$Anrede)
#Rothes, the Countess. of (Lucy Noel Martha Dyer-Edwards) wurde mit "Mrs" ausgetauscht.
daten$Anrede[760]<- "Mrs"
table(daten$Anrede)

#1b)
daten$Survived <-  as.factor(daten$Survived)
daten$Sex <- as.factor(daten$Sex)
daten$Embarked <- as.factor(daten$Embarked)

#1c)
daten$Pclass <- as.ordered(daten$Pclass)

#1d)

# Berechnet das Mittelalter von Personen mit Anrede x (ohne Beruecksichtigung von NAs)
mean_among <- function(x) {
  return(mean(daten$Age[daten$Anrede == x], na.rm = TRUE))
}

# Erstellt einen Vektor mit Mittelaltern, gruppiert bei Anrede
means <- sapply(c("Master", "Mr", "Mrs", "Ms"), mean_among)
names(means) <- c("Master", "Mr", "Mrs", "Ms")
means
# Master        Mr       Mrs        Ms 
# 4.472222 32.966427 35.991071 21.825503 

# Wenn bei i-tem Element in daten$Age NA steht, gibt ein entsprechendes Mittelalter zurueck
# Wenn i-tes Element kein NA ist, dann gibt das i-te Element wieder zurueck
replace_NA_with_mean <- function(i) {
  if (is.na(daten$Age[i])) {
    anrede <- daten$Anrede[i]
    return(means[anrede])
  }
  else return(daten$Age[i])
}

# Anwendet die Funktion replace_NA_with_mean auf alle Elemente von daten$Age und ersetzt NAs mit Mittelwerte aus means
for (i in 1:nrow(daten)) {
  daten$Age[i] <- replace_NA_with_mean(i)
}

#1e)
# Die Funktion gibt "Steuerbord" für die Passagiere mit ungeraden Kabinennummer und "Backboard" für die Passagiere mit geraden Kabinennummer zurueck. Ist die Kabinennummer unbekannt, liefert die Funktion eine NA.
bord_type <- function(cabin) {
  if (cabin == "") return(NA)
  else {
    cabin_number <- as.integer(substring(cabin, 2))
    return(ifelse(cabin_number %% 2, "Steuerbord", "Backbord"))
  }
}

# Hier wird die Funktion bord_type auf alle Eintraege in daten$Cabin angewendet.
daten$Bord <- sapply(daten$Cabin, bord_type)


# Die Funktion deck_type bestimmt den ersten Symbol in Kabinennummer, also die Deckzeichnung.
deck_type <- function(cabin) {
  if (cabin == "") return(NA)
  else return(substring(cabin, 1, 1))
}

# Hier wird die Funktion deck_type auf alle Eintraege in daten$Cabin angewendet.
daten$Deck <- sapply(daten$Cabin, deck_type)

#1f)
# Die Variablen PassengerId, Name, Ticket und Cabin aus dem Datensatz entfernen
daten$PassengerId <- NULL
daten$Name <- NULL
daten$Ticket <- NULL
daten$Cabin <- NULL


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

# Funktion anwenden(auf gewünschten Wert ändern)
ergebnis <- Haeufigkeitstabelle(daten$Anrede)


