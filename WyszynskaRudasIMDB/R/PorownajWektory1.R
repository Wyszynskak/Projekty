#'
#'Porownanie dwa wektory
#'
#'Funkcja \code{PorownajWektory1} bada podobienstwo dwoch wektorow
#'
#'@usage PorownajWektory1(w1, w2)
#'
#'@param w1, w2 - dwa wektory typu character
#'
#'@details Funkcja oblicza dlugosci obu wektorow, najpierw liczymy sume (w sensie mnogosciowym)
#'dlugosci wektorów, a takze dlugosc przeciecia wektorow. nastepnie dzielimy dlugosc przeciecia przez
#'sume dlugosci wektorow i to wszystko odejmujemy od 1. Ta miare zastosujemy dla gatunkow i krajow.
#'Stosujemy ja aby zabezpieczyc sie przed przypadkiem gdy jeden film ma np. tylko gatunek Drama, a drugi 
#'duzo gatunkow (w tym Drama). Niezaleznie od tego ile gatunkow bedziemy mieli w drugim filmie 
#'wczesniejsza funkcja (PorownajWektory) zwracalaby zawsze 0.
#'
#'
#'@return Zwraca liczbe z przedzialu [0, 1] wyznaczajaca odleglosc miedzy dwoma
#'wektorami. Zero w przypadku, gdy wektory bardzo
#'podobne, jeden gdy tego podobienstwa w ogole nie ma.
#'
#'@author Karolina Wyszynska
#'
#'@examples
#'wektor1 <- c("Drama","Crime")
#'wektor2 <- c("Drama", "Adventure")
#'
#'PorownajWektory1(wektor1, wektor2)
#'

PorownajWektory1 <- function(x1, x2){

  n1 <- length(x1)
  n2 <- length(x2)
  n12 <- length(intersect(x1,x2))
  
  1 - (n12 / (n1 + n2 - n12)) 
  
}


