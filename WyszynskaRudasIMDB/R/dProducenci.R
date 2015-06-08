#'
#' Liczenie odleglosci dla prodocentow
#'
#' Funkcja \code{dProducenci} liczy odleglosc miedzy producentami dwoch filmow z bazy
#'
#' @param IDFilm1 - id pierwszego filmu
#' @param IDFilm2 - id drugiego filmu
#' @param sciezkaDoBazy - sciezka do bazy filmow
#'
#'@details Funkcja wyciaga ID dwoch filmow z bazy. Jesli ktorys z nich nie ma producentow odleglosc miedzy
#' nimi wynosi 0.8, jesli tak nie jest miara jest liczona zgodnie z funkcja PorownajWektory.
#'@return Funkcja zwraca wartosc liczbowa opisujaca odleglosc miedzy filmami.
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' #Nie wywoluj jesli nie masz takiej bazy
#' dProducenci(4000,2461,"BazaFilmow.sql")
#' dProducenci(23456,2461,"BazaFilmow.sql")
#'
#'@import RSQLite
#'
dProducenci<-function(IDFilm1,IDFilm2,sciezkaDoBazy)
{
   baza_filmow<-dbConnect(SQLite(),dbname=sciezkaDoBazy)
   komenda1<-paste0("select Nazwa from Producenci where IDFilm=",IDFilm1)
   komenda2<-paste0("select Nazwa from Producenci where IDFilm=",IDFilm2)
   #Wyciagam prodocentow pierwszego
   producenci1 <- dbGetQuery(baza_filmow,komenda1)
   producenci1<-unlist(producenci1)
   names(producenci1)<-NULL
   #Wyciagam prodocentow drugiego
   producenci2 <- dbGetQuery(baza_filmow,komenda2)
   producenci2<-unlist(producenci2)
   names(producenci2)<-NULL
   #Jesli braki danych miara bedzie 0.8, jesli nie porownuj wektory
   if(length(producenci1)==0||length(producenci2)==0)
   {
      miara<-0.8
   }
   else
   {
      miara<-PorownajWektory(producenci1,producenci2)
   }


   return(miara)



   dbDisconnect(baza_filmow)
}
