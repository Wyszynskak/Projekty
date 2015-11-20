#Wczytanie danych 
dane <- read.table("file:///D:/Pobrane/FCZ.csv", sep=";",h=T)

# Potrzebuje zostawic czesc proby do testowania - odfiltowuje losowo 1/3 proby
tmp <- sample(1:nrow(dane),ceiling(nrow(dane)/3))
testy <- dane[tmp, ]
dane <- dane[-tmp, ]
#Zakladam, ze pierwsze 20 pytan jest skorelowanych, wiec dziele na 2 czesci
czesc1 <- dane[ ,1:10]
czesc2 <- dane[ ,11:20]

#Model regresji wielowymiarowej to tak naprawde odpowiednio wiele razy przeliczenie modelu 
#regresji jednowymiarowej
# Chce potraktowac czesc 1 jako odpowiedzi, a czesc2 jako predyktory
residua <- vector(mode="list",ncol(czesc1)) #residua
B <- matrix(rep(0,ncol(czesc1)*(ncol(czesc2)+1)),ncol=ncol(czesc1)) #Macierz wspolczynnikow w modelu wielowymiarowym

for(i in 1:ncol(czesc1)){
  y <- czesc1[,i]
  dane_model <- cbind(y,czesc2)
  model <- lm(y~.,data=dane_model)
  B[ ,i] <- model$coefficients
  residua[[i]] <- model$residuals
}

#Losowe odpowiedzi - zakladamy rozklad Bern(liczba_pytan, 0.5)
liczba_pytan <- 120
losowa_odpowiedz <- sample(c(0,1), size=liczba_pytan, replace=TRUE)
