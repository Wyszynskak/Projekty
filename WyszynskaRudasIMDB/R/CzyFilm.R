#' Sprawdzenie czy link prowadzi do filmu
#'
#' Funkcja \code{CzyFilm} sprawdza czy pod podanym linkiem znajduje sie 
#' informacja o filmie
#'
#' @usage
#' \code{CzyFilm(linkDoFilmu)}
#'
#' @param linkDoFilmu link do interesujacego nas filmu w serwisie IMBD (
#' struktura jak w przykladzie)
#'
#' @return Zwraca wartosc TRUE jesli pod linkiem znajduje sie film, FALSE
#' wraz z komentarzem w przeciwnym przypadku.
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' CzyFilm("http://www.imdb.com/title/tt0395699/")
#'

CzyFilm <- function(linkDoFilmu){
  strona <- readLines(linkDoFilmu, n =100)
  wykryj <- unlist(stri_match_all_regex(strona, 
                                        "(?<=og:type' content=\\\"video[.])[a-z]{5}"))
  wykryj <- wykryj[which(!is.na(wykryj))]
  if(length(wykryj)==0){print("To nie film!"); return(FALSE)}
  if(wykryj!="movie"){print("To nie film!"); return(FALSE)}
  TRUE
}