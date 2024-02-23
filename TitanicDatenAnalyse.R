#Gibt die Anzahl der fehlenden Einträge aus
sum(is.na(daten$Survived))
sum(is.na(daten$Pclass))
sum(is.na(daten$Sex))
sum(is.na(daten$Age))
sum(is.na(daten$SibSp))
sum(is.na(daten$Parch))
sum(is.na(daten$Fare))
sum(is.na(daten$Embarked))
sum(is.na(daten$Anrede))
sum(is.na(daten$Bord))
sum(is.na(daten$Deck))

#Gibt evlt interessante Werte für alle metrischen Merkmale aus
deskriptive_stat(daten$Age)
deskriptive_stat(daten$SibSp)
deskriptive_stat(daten$Fare)

#Gibt evlt interessante Werte für alle Kategorialen Merkmale aus
ergebnis<-Haeufigkeitstabelle(daten$Survived)
ergebnis<-Haeufigkeitstabelle(daten$Pclass)
ergebnis<-Haeufigkeitstabelle(daten$Sex)
ergebnis<-Haeufigkeitstabelle(daten$Parch)
ergebnis<-Haeufigkeitstabelle(daten$Embarked)
ergebnis<-Haeufigkeitstabelle(daten$Anrede)
ergebnis<-Haeufigkeitstabelle(daten$Bord)
ergebnis<-Haeufigkeitstabelle(daten$Deck)#Eigentlich nicht sinnvoll

#Lässt Survived gegen alle passenden Merkmale laufen
bivariate_stat(daten$Survived, daten$Pclass)
bivariate_stat(daten$Survived, daten$Sex)
bivariate_stat(daten$Survived, daten$Parch)
bivariate_stat(daten$Survived, daten$Embarked)
bivariate_stat(daten$Survived, daten$Anrede)
bivariate_stat(daten$Survived, daten$Bord)
bivariate_stat(daten$Survived, daten$Deck)#Eigentlich nicht sinnvoll

#Aus eigenem Interesse
bivariate_stat(daten$Pclass, daten$Sex)
bivariate_stat(daten$Pclass, daten$Deck)#Eigentlich nicht sinnvoll

#Lässt Surived gegen die metrischen Merkmale laufen
katbinom(daten$Survived, daten$Age)
katbinom(daten$Survived, daten$SibSp)
katbinom(daten$Survived, daten$Fare)
katbinom(daten$Sex, daten$Fare)#Aus eigenem Interesse

#Erstellt Mosaikplots 
visualisierung(daten$Survived, daten$Sex, daten$Parch,"Survived","Sex","Parch")
visualisierung(daten$Survived, daten$Sex, daten$Pclass,"Survived","Sex","Pclass")

#Erstellt Histogramme 
plot_grouped_histogram("Fare", "Survived", legend_names = c("Tod", "Lebendig"), main = "Preis nach Überleben")
plot_grouped_histogram("Age", "Pclass", legend_names = c("Class1", "Class2", "class3"), main = "Alter nach Klasse")
plot_grouped_histogram("Age", "Survived", legend_names = c("Tod", "Lebendig"), main = "Alter nach Überleben")
