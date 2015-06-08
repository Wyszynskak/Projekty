#'
#'Porownanie ocen dwoch filmow
#'
#'Funkcja \code{PorownajOceny} porownuje oceny dwoch filmow
#'
#'@usage PorownajOceny(o1, o2)
#'
#'@param o1,o2 - oceny filmow jako wartosci typu numeric
#'
#'@details Oceny, ktore mozna przyznac poszczegolnym filmom zawieraja sie w
#'przedziale [1, 10], dlatego maksymalna odleglosc miedzy dwoma ocenami wynosi 9.
#'Stad funkcja \code{PorownajOceny} oblicza zwykla odleglosc miedzy miedzy dwiema
#'ocenami, a nastepnie normuje ja dzielac przez 9.
#'
#'@return Zwraca liczbe z przedzialu [0, 1] wyznaczajaca odleglosc miedzy dwoma
#'filmami (liczona na podstawie oceny). Zero w przypadku, gdy filmy bardzo
#'podobne, jeden gdy tego podobienstwa w ogle nie ma.
#'
#'@author Karolina Wyszynska
#'
#'@examples
#'ocena1 <- 8.6
#'ocena2 <- 7.5
#'
#'PorownajOceny(ocena1, ocena2)
#'


PorownajOceny <- function(o1, o2){

  abs(o1 - o2)/9

}
