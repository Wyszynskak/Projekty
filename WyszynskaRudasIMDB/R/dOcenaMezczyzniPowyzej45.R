#'
#' Liczenie odleglosci dla  ocen mezczyzn od 45 lat wzwyz
#'
#' Funkcja \code{dOcenaMezczyzniPowyzej45} liczy odleglosc miedzy ocenami mezczyzn od 45 lat wzwyz dla
#' dwoch filmow z bazy
#'
#' @param IDFilm1 - id pierwszego filmu
#' @param IDFilm2 - id drugiego filmu
#' @param sciezkaDoBazy - sciezka do bazy filmow
#'
#'@details Funkcja wyciaga ID dwoch filmow z bazy. Jesli ktorys z nich nie ma ocena mezczyzn od 45 lat wzwyz
#'odleglosc miedzy nimi wynosi 0.5, jesli tak nie jest miara jest liczona zgodnie z funkcja PorownajOceny.
#'@return Funkcja zwraca wartosc liczbowa opisujaca odleglosc miedzy filmami.
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' #Nie wywoluj jesli nie masz takiej bazy
#' dOcenaMezczyzniPowyzej45(4000,2461,"BazaFilmow.sql")
#' dOcenaMezczyzniPowyzej45(23456,2461,"BazaFilmow.sql")
#'
#'@import RSQLite
#'

dOcenaMezczyzniPowyzej45<-function(IDFilm1,IDFilm2,sciezkaDoBazy)
{
   baza_filmow<-dbConnect(SQLite(),dbname=sciezkaDoBazy)

   #Tworzenie komend w SQL
   komenda1<-paste0("select MezczyzniPowyzej45 from Oceny where IDFilm=",IDFilm1)
   komenda2<-paste0("select MezczyzniPowyzej45 from Oceny where IDFilm=",IDFilm2)
   #Wyciagam ocene dla Mezczyzn Powyzej45 dla pierwszego
   ocena1 <- dbGetQuery(baza_filmow,komenda1)
   ocena1<-unlist(ocena1)
   names(ocena1)<-NULL
   #Wyciagam ocene dla Mezczyzn Powyzej45 dla drugiego
   ocena2<-dbGetQuery(baza_filmow,komenda2)
   ocena2<-unlist(ocena2)
   names(ocena2)<-NULL
   if(ocena1==(-1)||ocena2==(-1))
   {
      miara<-0.5
   }
   else
   {
      miara<-PorownajOceny(ocena1,ocena2)
   }
   return(miara)
   dbDisconnect(baza_filmow)


}
