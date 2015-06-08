#'
#'Wagi odpowiadajace preferencjom uzytkownika
#'
#'Funkcja \code{WektorWspolczynnikow} oblicza wektor wag z uwzglednieniem
#'preferencji uzytkownika
#'
#'@usage WektorWspolczynnikow(coWazne, plec, wiek)
#'
#'@param coWazne dwuelementowy wektor typu character podajacy dwie najwazniejsze
#'dla uzytkownika cechy filmu. Dostepne wartosci to: Rok, Fabula, Gatunki,
#'Scenarzysci, Rezyserowie, Kraje produkcji, Aktorzy, Gwiazdy i Producenci.
#'@param plec plec uzytkownika jako wartosc "K" jesli kobieta lub "M" jesli mezczyzna
#'@param wiek wiek uzytkownika podany w latach
#'
#'@details Funkcja oblicza wagi dla poszczegolnych cech filmow. Dwum wyroznionym
#' cechom przyporzadkowywuje wartosc 0.23, reszcie wartosc 0.06 . Dodatkowo
#' ceche ocena oblicza jako srednia artmetyczna oceny glownej oraz tej
#' odpowiadajacej wiekowi i plci uzytkowika
#'
#'@return Wektor typu double wag dla poszczegolnych kolumn tabeli Odleglosci
#'
#'@author Karolina Wyszynska
#'
#'@examples
#'wektorWspolczynnikow <- WektorWspolczynnikow(c("Oceny", "Producenci"), "K", 22)
#'
#'

WektorWspolczynnikow <- function(coWazne, plec, wiek){
  wektorWspolczynnikow <- c(rep(0.06, 10), rep(0,9))
  nazwy <- c("Rok", "Fabula", "Gatunki", "Scenarzysci", "Rezyserowie",
             "Kraje produkcji", "Aktorzy", "Gwiazdy", "Producenci", "Oceny")
  #Ustalamy, które oceny powinny mieć dodatni współczynnik
  if(plec=="M") k <- 0 else k <- 1

  if(wiek < 18) l <- 12 else{
    if(wiek < 29) l <- 14 else{
      if(wiek < 45) l <- 16 else{
        l <- 18}
    }
  }

  #Nadajemy wagę innym wspołczynnikom niż oceny
  if(coWazne[1]=="Oceny"){
    mnoznik <- 0.23
    wektorWspolczynnikow[which(nazwy==coWazne[2])] <- 0.23
  } else {
    if(coWazne[2]=="Oceny"){
      mnoznik <- 0.23
      wektorWspolczynnikow[which(nazwy==coWazne[1])] <- 0.23
    } else {
      mnoznik <- 0.06
      wektorWspolczynnikow[which(nazwy==coWazne[1])] <- 0.23
      wektorWspolczynnikow[which(nazwy==coWazne[2])] <- 0.23
    }
  }

  #Nadajemy wagę ocenom
  wektorWspolczynnikow[c(11, k + l)] <- mnoznik/2

  wektorWspolczynnikow
}
