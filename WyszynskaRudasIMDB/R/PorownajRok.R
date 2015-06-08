#'
#'Porownanie lat produkcji dwoch filmow
#'
#'Funkcja \code{PorownajRok} porownuje lata produkcji dwoch filmow
#'
#'@usage PorownajRok(r1, r2)
#'
#'@param r1,r2 - rok produkcji poszczegolnych filmow jako wartosci typu numeric
#'
#'@details Funkcja oblicza zwykla odleglosc miedzy dwoma latami, a nastepnie
#'dzieli ja przez 30. Uznalismy, ze 30 lat to najdluzej kiedy filmy moga byc
#'jeszcze uznane za podobne, dlatego dla wiekszych odleglosci funkcja zwraca 1.
#'
#'
#'@return Zwraca liczbe z przedzialu [0, 1] wyznaczajaca odleglosc miedzy dwoma
#'filmami (liczona na podstawie roku produkcji). Zero w przypadku, gdy filmy bardzo
#'podobne, jeden gdy tego podobienstwa w ogle nie ma.
#'
#'@author Karolina Wyszynska
#'
#'@examples
#'rok1 <- 1992
#'rok2 <- 2000
#'
#'PorownajRok(rok1, rok2)
#'
#'rok1 <- 1960
#'rok2 <- 2034
#'
#'PorownajRok(rok1, rok2)
#'


PorownajRok <- function(r1, r2){

  odleglosc <- abs(r1-r2)
  if(odleglosc > 30) return(1)

  odleglosc/30

}
