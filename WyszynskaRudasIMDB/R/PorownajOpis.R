#'
#'Porownanie opisow dwoch filmow
#'
#'Funkcja \code{PorownajOpis} porownuje opisy dwoch filmow (w jezyku angielskim)
#'
#'@usage PorownajOpis(o1, o2)
#'
#'@param o1,o2 - opisy filmow podane jako wektory typu character dlugosci 1
#'
#'@details Funkcja korzystajac z pakietow \code{tm} i \code{SnowballC}
#'oczyszcza tekst (musi byc w jezyku angielskim). Tak przetworzone opisy sa
#'rozbijane na dwa wektory slow. Wektory te sa nastepnie porownywane przy uzyciu
#' funkcji \code{PorownajWektory}
#'
#'@return Zwraca liczbe z przedzialu [0, 1] wyznaczajaca odleglosc miedzy dwoma
#'filmami (liczona na podstawie opisu). Zero w przypadku, gdy filmy bardzo
#'podobne, jeden gdy tego podobienstwa w ogle nie ma.
#'
#'
#'@author Karolina Wyszynska
#'
#'@examples
#'opis1 <- "This time Ellena falls in love with Richard and they live happily
#'togehter ever after."
#'opis2 <- "Cathrene get engaged with Tom and they live happily together."
#'
#'PorownajOpis(opis1, opis2)
#'
#'@import tm, SnowballC, stringi, dplyr
#'

PorownajOpis <- function(o1, o2){

  #Korzystamy z pakietu tm do pracy z tekstem

  #Tworzymy korpus
  corp <- Corpus(VectorSource(c(o1, o2)))

  corp <- WyczyscTekst(corp)

  #Tworze wektory słów
  tekst <- (unlist(corp))
  ktore <- which(names(tekst)=="content.content")
  tekst <- tekst[ktore]
  w1 <- stri_extract_all_words(tekst[1]) %>% unlist %>% unique
  w2 <- stri_extract_all_words(tekst[2]) %>% unlist %>% unique
  PorownajWektory(w1, w2)
}

WyczyscTekst <- function(corp){

  #Usuwamy białe znaki i interpunkcję
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, removePunctuation)

  #Zamieniamy na małe litery
  corp <- tm_map(corp, content_transformer(tolower))

  #Usuwamy stopwordsy i końcówki wyrazów
  corp <- tm_map(corp, removeWords, stopwords("english"))
  tm_map(corp, stemDocument)

}


