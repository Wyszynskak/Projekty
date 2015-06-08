#'
#' Liczenie odleglosci dla aktorow z glownych stron filmow
#'
#' Funkcja \code{dAktorzyGlowni} liczy odleglosc miedzy aktorami z glownych
#' stron filmow dwoch filmow z bazy
#'
#' @param IDFilm1 - id pierwszego filmu
#' @param IDFilm2 - id drugiego filmu
#' @param sciezkaDoBazy - sciezka do bazy filmow
#'
#'@details Funkcja wyciaga ID dwoch filmow z bazy. Jesli ktorys z nich nie ma aktorow na swojej glownej
#'stronie odleglosc miedzy nimi wynosi 0.8, jesli tak nie jest miara jest liczona zgodnie z funkcja
#'PorownajWektory.
#'@return Funkcja zwraca wartosc liczbowa opisujaca odleglosc miedzy filmami.
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' #Nie wywoluj jesli nie masz takiej bazy
#' dAktorzyGlowni(4000,2461,"BazaFilmow.sql")
#' dAktorzyGlowni(23456,2461,"BazaFilmow.sql")
#'
#'@import RSQLite, stringi


dAktorzyGlowni<-function(IDFilm1,IDFilm2,sciezkaDoBazy)
{
   baza_filmow<-dbConnect(SQLite(),dbname=sciezkaDoBazy)


   #Tworzenie komend w SQL
   komenda1<-paste0("select Imie,Nazwisko,czyGlowny from Aktorzy where IDFilm=",IDFilm1)
   komenda2<-paste0("select Imie,Nazwisko,czyGlowny from Aktorzy where IDFilm=",IDFilm2)

   #Wyciagam ramki danych o kolumnach imie, nawsko i czyGlowny
   nazwa1 <- dbGetQuery(baza_filmow,komenda1)
   nazwa1<-nazwa1[nazwa1$czyGlowny==1,c(1,2)]
   imiona_i_nazwiska1<-stri_paste(nazwa1[,1],nazwa1[,2],sep=" ")
   nazwa2 <- dbGetQuery(baza_filmow,komenda2)
   nazwa2<-nazwa2[nazwa2$czyGlowny==1,c(1,2)]
   imiona_i_nazwiska2<-stri_paste(nazwa2[,1],nazwa2[,2],sep=" ")
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
