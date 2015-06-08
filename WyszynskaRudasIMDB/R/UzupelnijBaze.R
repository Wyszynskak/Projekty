#'Uzupelnia baze danych
#'
#'Funkcja \code{UzupelnijBaze} jest 'ladnym opakowaniem' na funkcje
#'\code{WstawFilmDoBazy}. Przyjmuje wektor linkow i po koleji
#'umieszcza je w bazie za pomoca wczesniej wspomnianiej funkcji
#'
#'@usage
#'\code{UzupelnijBaze(listaLinkow, sciezkaDoBazy)}
#'
#'@param listaLinkow wektor typu character zawierajacy linki do filmow,
#'ktore chcemy wstawic do bazy
#'@param sciezkaDoBazy sciezka do pliku z baza danych
#'
#'@author Karolina Wyszynska
#'
#'@examples
#'UzupelnijBaze(c("http://www.imdb.com/title/tt0395699/",
#'                "http://www.imdb.com/title/tt0006033/"),
#'                "nazwaBazy.sql")
#'


UzupelnijBaze <- function(listaLinkow, sciezkaDoBazy){
  suppressWarnings(lapply(listaLinkow, WstawFilmDoBazy, sciezkaDoBazy))
  TRUE
}
