#'
#' Liczenie odleglosci dla rezyserow
#'
#' Funkcja \code{dRezyserowie} liczy odleglosc miedzy rezyserami dwoch filmow z bazy
#'
#' @param IDFilm1 - id pierwszego filmu
#' @param IDFilm2 - id drugiego filmu
#' @param sciezkaDoBazy - sciezka do bazy filmow
#'
#'@details Funkcja wyciaga ID dwoch filmow z bazy. Jesli ktorys z nich nie ma rezyserow odleglosc
#' miedzy nimi wynosi 0.8, jesli tak nie jest miara jest liczona zgodnie z funkcja PorownajWektory.
#'@return Funkcja zwraca wartosc liczbowa opisujaca odleglosc miedzy filmami.
#'
#'@author Krzysztof Rudas
#'
#'
#'@import RSQLite, stringi
#'
#'@examples
#' #Nie wywoluj jesli nie masz takiej bazy
#' dRezyserowie(4000,2461,"BazaFilmow.sql")
#' dRezyserowie(23456,2461,"BazaFilmow.sql")


dRezyserowie<-function(IDFilm1,IDFilm2,sciezkaDoBazy)
{
   baza_filmow<-dbConnect(SQLite(),dbname=sciezkaDoBazy)

   #Tworzenie komend w SQL
   komenda1<-paste0("select Imie,Nazwisko from Rezyserowie where IDFilm=",IDFilm1)
   komenda2<-paste0("select Imie,Nazwisko from Rezyserowie where IDFilm=",IDFilm2)


   #Wyciagam imiona i nazwiska rezyserow pierwszego
   nazwa1 <- dbGetQuery(baza_filmow,komenda1)
   #Wyciagam imiona i nazwiska rezyserow drugiego
   nazwa2 <- dbGetQuery(baza_filmow,komenda2)
   #Robimy wektor napisow w formie imie i nazwisko dla pierwszego
   imiona_i_nazwiska1<-stri_paste(nazwa1[,1],nazwa1[,2], sep=" ")
   #Robimy wektor napisow w formie imie i nazwisko dla drugiego
   imiona_i_nazwiska2<-stri_paste(nazwa2[,1],nazwa2[,2], sep=" ")
   if(nrow(nazwa1)==0||nrow(nazwa2)==0)
   {
      miara<-0.8
   }
   else
   {
      miara<-PorownajWektory(imiona_i_nazwiska1,imiona_i_nazwiska2)
   }


   return(miara)


   dbDisconnect(baza_filmow)
}
