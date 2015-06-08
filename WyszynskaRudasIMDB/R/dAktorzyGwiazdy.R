#'
#' Liczenie odleglosci dla aktorow bedacych gwiazdami
#'
#' Funkcja \code{dAktorzyGwiazdy} liczy odleglosc miedzy aktorami bedacymi gwiazdami
#' z dwoch filmow z bazy
#'
#' @param IDFilm1 - id pierwszego filmu
#' @param IDFilm2 - id drugiego filmu
#' @param sciezkaDoBazy - sciezka do bazy filmow
#'
#'@details Funkcja wyciaga ID dwoch filmow z bazy. Jesli ktorys z nich nie ma aktorow bedacych gwiazdami
#' odleglosc miedzy nimi wynosi 0.8, jesli tak nie jest miara jest liczona zgodnie z funkcja
#' PorownajWektory.
#'@return Funkcja zwraca wartosc liczbowa opisujaca odleglosc miedzy filmami.
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' #Nie wywoluj jesli nie masz takiej bazy
#' dAktorzyGwiazdy(4000,2461,"BazaFilmow.sql")
#' dAktorzyGwiazdy(23456,2461,"BazaFilmow.sql")
#'
#'@import RSQLite, stringi
#'

dAktorzyGwiazdy<-function(IDFilm1,IDFilm2,sciezkaDoBazy)
{
   baza_filmow<-dbConnect(SQLite(),dbname=sciezkaDoBazy)


   #Tworzenie komend w SQL
   komenda1<-paste0("select Imie,Nazwisko,czyGwiazda from Aktorzy where IDFilm=",IDFilm1)
   komenda2<-paste0("select Imie,Nazwisko,czyGwiazda from Aktorzy where IDFilm=",IDFilm2)

   #Wyciagam ramki danych o kolumnach imie, nazwisko i czyGwiazda
   nazwa1 <- dbGetQuery(baza_filmow,komenda1)
   nazwa1<-nazwa1[nazwa1$czyGwiazda==1,c(1,2)]
   imiona_i_nazwiska1<-stri_paste(nazwa1[,1],nazwa1[,2],sep=" ")
   nazwa2 <- dbGetQuery(baza_filmow,komenda2)
   nazwa2<-nazwa2[nazwa2$czyGwiazda==1,c(1,2)]
   imiona_i_nazwiska2<-stri_paste(nazwa2[,1],nazwa2[,2],sep=" ")
   if(nrow(nazwa1)==0||nrow(nazwa2)==0)
   {
      miara<-0.8
   }
   else
   {
      miara<-PorownajWektory(imiona_i_nazwiska1,imiona_i_nazwiska2)
   }

   dbDisconnect(baza_filmow)
   return(miara)



}
