#'
#'Porownanie dwa wektory
#'
#'Funkcja \code{PorownajWektory} bada podobienstwo dwoch wektorow
#'
#'@usage PorownajWektory(w1, w2)
#'
#'@param w1, w2 - dwa wektory typu character
#'
#'@details Funkcja oblicza dlugosci obu wektorow, a nastepnie wyciaga minimum z
#'tych wielkosci i liczby 6 (uznalismy, ze 6 wspolnych aktorow to wystarczajace podobienstwo). Nastepnie
#'okresla dlugosc przeciecia obu wektorow. Normuje ja przez wczesniej wyliczona
#'wielkosc i oblicza odleglosc od 1.
#'
#'
#'@return Zwraca liczbe z przedzialu [0, 1] wyznaczajaca odleglosc miedzy dwoma
#'wektorami. Zero w przypadku, gdy wektory bardzo
#'podobne, jeden gdy tego podobienstwa w ogle nie ma.
#'
#'@author Karolina Wyszynska
#'
#'@examples
#'wektor1 <- c("Adam Sandler","John Turturro","Emmanuelle Chriqui")
#'wektor2 <- c("Adam Sandler", "Drew Barrymore", "Rob Schneider")
#'
#'PorownajWektory(wektor1, wektor2)
#'

PorownajWektory <- function(w1, w2){
  n1 <- length(w1)
  n2 <- length(w2)

  dlugosc <- min(n1, n2, 6)
  n12 <- length(intersect(w1, w2))

  if(n12 > 6) n12 <- 6

  1 - n12/dlugosc

}

