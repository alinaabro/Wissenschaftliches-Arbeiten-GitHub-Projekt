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
daten$Pclass <- as.ordered(daten$Pclass)
