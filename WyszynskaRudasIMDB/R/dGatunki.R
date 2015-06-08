#'
#' Liczenie odleglosci dla gatunkow
#'
#' Funkcja \code{dGatunki} liczy odleglosc miedzy gatunkami dwoch filmow z bazy
#'
#' @param IDFilm1 - id pierwszego filmu
#' @param IDFilm2 - id drugiego filmu
#' @param sciezkaDoBazy - sciezka do bazy filmow
#'
#'@details Funkcja wyciaga ID dwoch filmow z bazy. Jesli ktorys z nich nie ma gatunkow odleglosc miedzy
#' nimi wynosi 0.8, jesli tak nie jest miara jest liczona zgodnie z funkcja PorownajWektory.
#'@return Funkcja zwraca wartosc liczbowa opisujaca odleglosc miedzy filmami.
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' #Nie wywoluj jesli nie masz takiej bazy
#' dGatunki(4000,2461,"BazaFilmow.sql")
#' dGatunki(23456,2461,"BazaFilmow.sql")
#'
#'@import RSQLite
#'

dGatunki<-function(IDFilm1,IDFilm2,sciezkaDoBazy)
{
   baza_filmow<-dbConnect(SQLite(),dbname=sciezkaDoBazy)

   komenda1<-paste0("select Nazwa from Gatunki where IDFilm=",IDFilm1)
   komenda2<-paste0("select Nazwa from Gatunki where IDFilm=",IDFilm2)
   #Wyciagam gatunki pierwszego
   gatunki1 <- dbGetQuery(baza_filmow,komenda1)
   gatunki1<-unlist(gatunki1)
   names(gatunki1)<-NULL
   #Wyciagam gatunki drugiego
   gatunki2 <- dbGetQuery(baza_filmow,komenda2)
   gatunki2<-unlist(gatunki2)
   names(gatunki2)<-NULL
   #Jesli braki danych miara bedzie 0.8, jesli nie porownuj wektory
   if(length(gatunki1)==0||length(gatunki2)==0)
   {
      miara<-0.8
   }
   else
   {
      miara<-PorownajWektory1(gatunki1,gatunki2)
   }


   return(miara)



   dbDisconnect(baza_filmow)

}
