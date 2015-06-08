#'
#' Liczenie odleglosci dla krajow
#'
#' Funkcja \code{dKraje} liczy odleglosc miedzy krajami dwoch filmow z bazy
#'
#' @param IDFilm1 - id pierwszego filmu
#' @param IDFilm2 - id drugiego filmu
#' @param sciezkaDoBazy - sciezka do bazy filmow
#'
#'@details Funkcja wyciaga ID dwoch filmow z bazy. Jesli ktorys z nich nie ma krajow produkcji odleglosc
#' miedzy nimi wynosi 0.8, jesli tak nie jest miara jest liczona zgodnie z funkcja PorownajWektory.
#'@return Funkcja zwraca wartosc liczbowa opisujaca odleglosc miedzy filmami.
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' #Nie wywoluj jesli nie masz takiej bazy
#' dKraje(4000,2461,"BazaFilmow.sql")
#' dKraje(23456,2461,"BazaFilmow.sql")
#'
#'@import RSQLite
#'
#'

dKraje<-function(IDFilm1,IDFilm2,sciezkaDoBazy)
{
   baza_filmow<-dbConnect(SQLite(),dbname=sciezkaDoBazy)

   komenda1<-paste0("select Nazwa from Kraje where IDFilm=",IDFilm1)
   komenda2<-paste0("select Nazwa from Kraje where IDFilm=",IDFilm2)
   #Wyciagam kraje pierwszego
   kraje1 <- dbGetQuery(baza_filmow,komenda1)
   kraje1<-unlist(kraje1)
   names(kraje1)<-NULL
   #Wyciagam kraje drugiego
   kraje2 <- dbGetQuery(baza_filmow,komenda2)
   kraje2<-unlist(kraje2)
   names(kraje2)<-NULL
   #Jesli braki danych miara bedzie 0.8, jesli nie porownuj wektory
   if(length(kraje1)==0||length(kraje2)==0)
   {
      miara<-0.8

   }
   else
   {
      miara<-PorownajWektory1(kraje1,kraje2)
   }


   return(miara)

   dbDisconnect(baza_filmow)
}
