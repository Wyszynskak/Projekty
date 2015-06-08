#'
#' Sprawdzenie, czy dane ID jest w bazie.
#'
#' Funkcja \code{SprawdzCzywBazie} sprawdza, czy para filmow o podanych przez użytkownika ID
#' jest w bazie filmow.
#'
#' @param IDFilm1 - id pierwszego filmu
#' @param IDFilm2 - id drugiego filmu
#' @param baza_filmow - polaczenie z bazą filmow
#'
#'@details Funkcja korzysta juz z polaczenia z baza filmow. W tabeli Filmy, sprawdza czy istnieja zadane
#'w parametrach ID filmow.
#'@return Funkcja zwraca wartosc TRUE jesli obydwa ID znajduja sie w bazie, jesli nie to mamy wartosc FALSE.
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' #Nie wywoluj jesli nie masz takiej bazy
#' SprawdzCzywBazie(4000,2461,dbConnect(SQLite(),dbname="BazaFilmow.sql"))
#' SprawdzCzywBazie(900,2461,dbConnect(SQLite(),dbname="BazaFilmow.sql"))
#'
#'@import RSQLite
#'
#'


SprawdzCzywBazie<-function(IDFilm1,IDFilm2,baza_filmow)
{
   #Tworzenie komend w SQL
   komenda_czy_w_bazie1<-paste0("select IDFilm from Filmy where IDFilm=",IDFilm1)
   komenda_czy_w_bazie2<-paste0("select IDFilm from Filmy where IDFilm=",IDFilm2)
   czy_w_bazie1<-dbGetQuery(baza_filmow,komenda_czy_w_bazie1)
   czy_w_bazie2<-dbGetQuery(baza_filmow,komenda_czy_w_bazie2)
   if(nrow(czy_w_bazie1)==0||nrow(czy_w_bazie2)==0)
   {
      return(FALSE)
   }
   else
   {
      return(TRUE)
   }
}
