#'
#' Liczenie odleglosci dla opisow
#'
#' Funkcja \code{OpisIMDB} liczy odleglosc miedzyopisami dwoch filmow z bazy
#'
#' @param IDFilm1 - id pierwszego filmu
#' @param IDFilm2 - id drugiego filmu
#' @param sciezkaDoBazy - sciezka do bazy filmow
#'
#'@details Funkcja wyciaga ID dwoch filmow z bazy. Jesli ktorys z nich nie ma opisu odleglosc
#' miedzy nimi wynosi 0.8, jesli tak nie jest miara jest liczona zgodnie z funkcja PorownajOpis.
#'@return Funkcja zwraca wartosc liczbowa opisujaca odleglosc miedzy filmami.
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' #Nie wywoluj jesli nie masz takiej bazy
#' dOpisIMDB(4000,2461,"BazaFilmow.sql")
#' dOpisIMDB(23456,2461,"BazaFilmow.sql")
#'
#'@import RSQLite

dOpisIMDB<-function(IDFilm1,IDFilm2,sciezkaDoBazy)
{
   baza_filmow<-dbConnect(SQLite(),dbname=sciezkaDoBazy)

   #Tworzenie komend w SQL
   komenda1<-paste0("select OpisIMDB from Filmy where IDFilm=",IDFilm1)
   komenda2<-paste0("select OpisIMDB from Filmy where IDFilm=",IDFilm2)
   #Wyciagam opis dla pierwszego
   opis1 <- dbGetQuery(baza_filmow,komenda1)
   opis1<-unlist(opis1)
   names(opis1)<-NULL
   #Wyciagam opis dla drugiego
   opis2<-dbGetQuery(baza_filmow,komenda2)
   opis2<-unlist(opis2)
   names(opis2)<-NULL
   if(length(na.omit(opis1))==0||length(na.omit(opis2))==0)
   {
      miara<-0.8
   }
   else
   {
      miara<-PorownajOpis(opis1,opis2)
   }

   return(miara)
   dbDisconnect(baza_filmow)


}
