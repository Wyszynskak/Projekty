#'
#' Liczenie odleglosci dla lat produkcji
#'
#' Funkcja \code{dRokProdukcji} liczy odleglosc miedzy latami produkcji dwoch filmow z bazy
#'
#' @param IDFilm1 - id pierwszego filmu
#' @param IDFilm2 - id drugiego filmu
#' @param sciezkaDoBazy - sciezka do bazy filmow
#'
#'@details Funkcja wyciaga ID dwoch filmow z bazy. Jesli ktorys z nich nie ma roku produkcji odleglosc
#' miedzy nimi wynosi 0.8, jesli tak nie jest miara jest liczona zgodnie z funkcja PorownajRok.
#'@return Funkcja zwraca wartosc liczbowa opisujaca odleglosc miedzy filmami.
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' #Nie wywoluj jesli nie masz takiej bazy
#' dRokProdukcji(4000,2461,"BazaFilmow.sql")
#' dRokProdukcji(23456,2461,"BazaFilmow.sql")
#'
#'@import RSQLite
#'


dRokProdukcji<-function(IDFilm1,IDFilm2,sciezkaDoBazy)
{
   baza_filmow<-dbConnect(SQLite(),dbname=sciezkaDoBazy)

   #Tworzenie komend w SQL
   komenda1<-paste0("select RokProdukcji from Filmy where IDFilm=",IDFilm1)
   komenda2<-paste0("select RokProdukcji from Filmy where IDFilm=",IDFilm2)
   #Wyciagam date produkcji dla pierwszego
   rok1 <- dbGetQuery(baza_filmow,komenda1)
   rok1<-unlist(rok1)
   names(rok1)<-NULL
   #Wyciagam date produkcji dla drugiego
   rok2<-dbGetQuery(baza_filmow,komenda2)
   rok2<-unlist(rok2)
   names(rok2)<-NULL
   if(rok1==0||rok2==0)
   {
      miara<-0.8
   }
   else
   {
      miara<-PorownajRok(rok1,rok2)
   }
   return(miara)



}
