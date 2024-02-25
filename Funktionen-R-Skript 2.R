#Hilfefunktion fuer die Entropie
#Empirische Entropie berechnen
entropieBerechnen <- function(relH){
  freq <- relH / 100
  k <- length(freq)
  #Berechne Entropie nach Definition
  entropie <- sum(freq * log(1/freq)) / log(k)
  return(entropie)
}

#Hilfefunktion fuer das Berechnen von dem Minimum
#der Beobachtungen
minimum <- function(x){
  minimum <- x[1]
  n <- length(x)
  for(i in 2:n){
    if(x[i] < minimum) minimum <- x[i]
  }
  return(minimum)
}

#Und fuer das Maximum der Beobachtungen
maximum <- function(x){
  maximum <- x[1]
  n <- length(x)
  for(i in 2:n){
    if(x[i] > maximum) maximum <- x[i]
  }
  return(maximum)
}

#Funktion fuer das Berechnen des arithmetischen Mittels
arit_mittel <- function(x){
  b <- 0 
  n <- length(x)
  for(i in 1:n){
    b <- b+x[i]
  }
  return(b/n)
}
