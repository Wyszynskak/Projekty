#'
#' Wrzucanie wyliczonych miar do pliku CSV
#'
#' Funkcja \code{WstawMiaryDoCSV} liczy wektory miar dla filmow i wrzuca je do pliku CSV
#'
#' @param IDFilm1 - id pierwszego filmu
#' @param IDFilm2 - id drugiego filmu
#' @param sciezkaDoBazy - sciezka do bazy filmow
#'
#'@details Funkcja jest rozszerzeniem funkcji \code{WstawOdlegloscDoCSV}. Dla zadanego ID filmu
#'pierwszego i filmu drugiego zlicza wektory odleglosci i zapisuje je do csv w nastepujacy sposob:
#'Najpierw dla wszystkich par filmow, gdzie pierwszym filmem jest film o ID rownym IDFilm1, a drugim
#'filmy o ID wiekszym od IDFilm2. Pozniej IDFilm1 zmienia sie na nastepne i liczone sa miary dla nowego
#'IDFilm1 i IDFilm2 wiekszych od IDFilm1, nastepnie znow IDFilm1 staje sie nastepnym w kolejnosci i proces
#'sie powtarza. (Nalezy podawac IDFilm2 wieksze lub rowne od IDFilm1).
#'
#'@author Krzysztof Rudas
#'
#'
#'@import RSQLite, stringi
#'
#'@examples
#' #Nie wywoluj jesli nie masz takiej bazy
#' WstawMiaryDoCSV(2461,4000,"BazaFilmow.sql")
#' WstawMiaryDoCSV(2461,23456,"BazaFilmow.sql")

WstawMiaryDoCSV<-function(IDFilm1,IDFilm2,sciezkaDoBazy)
{
   #Stworzmy plik do ktorego bedziemy pobierac/uaktywnijmy sciezke do istniejacej csv-ki
   sciezkaDoCSV <- "Odleglosci.csv"
   if(!file.exists(sciezkaDoCSV)){file.create(sciezkaDoCSV)}
   #polaczenie z baza
   baza_filmow<-dbConnect(SQLite(),dbname=sciezkaDoBazy)
   #komendy
   #daj id Film1 gdzie ostatnio skonczylismy
   komenda1<-paste0("select IDFilm from Filmy where IDFilm=",IDFilm1)
   #daj id wszystkich wiekszych od tego IDFilm2 gdzie skonczylismy
   komenda2<-paste0("select IDFilm from Filmy where IDFilm>",IDFilm2)
   #daj id wieksze od id Film1
   komenda3<-paste0("select IDFilm from Filmy where IDFilm>",IDFilm1)
   #realizacja komend
   IDFilm1 <- dbGetQuery(baza_filmow,komenda1)
   IDFilm2 <- dbGetQuery(baza_filmow,komenda2)
   IDFilm3 <- dbGetQuery(baza_filmow,komenda3)
   #zamiana na wektory i pozbycie sie nazw
   wektorIDFilm1<-unlist(IDFilm1)
   wektorIDFilm2<-unlist(IDFilm2)
   wektorIDFilm3<-unlist(IDFilm3)
   names(wektorIDFilm1)<-NULL
   names(wektorIDFilm2)<-NULL
   names(wektorIDFilm3)<-NULL
   #dlugosc wektora wektorIDFilm2
   dlugoscIDFilm2<-length(wektorIDFilm2)
   #przejdz po wszystkich IDFilm2 wiekszych od zadanego przy ustalonym IDFilm1
   if(dlugoscIDFilm2!=0)
   {
      for(i in 1:dlugoscIDFilm2)
      {
         WstawOdlegloscDoCSV(wektorIDFilm1,wektorIDFilm2[i],sciezkaDoBazy,sciezkaDoCSV)
         napis<-stri_paste("Wpisano miary dla filmow o IDFilm1=",as.character(wektorIDFilm1),
                           "i o IDFilm2=",as.character(wektorIDFilm2[i]),sep=" ")
      print(napis)
      } 
   }
   #Przeskakujemy z IDFilm1 do nastepnego jesli jeszcze taki jest
   while(length(wektorIDFilm3)>0)
   {
      wektorIDFilm1<-wektorIDFilm3[1]
      #aktualizacja wektora wiekszych od wektorIDFilm1
      wektorIDFilm3<-wektorIDFilm3[-1]
      #dlugosc wektora wektorIDFilm3
      dlugoscIDFilm3<-length(wektorIDFilm3)
      if(dlugoscIDFilm3!=0)
      {
         #Przejdz po wszystkich wiekszych
         for(i in 1:dlugoscIDFilm3)
         {
            WstawOdlegloscDoCSV(wektorIDFilm1,wektorIDFilm3[i],sciezkaDoBazy,sciezkaDoCSV)
            napis<-stri_paste("Wpisano miary dla filmow o IDFilm1=",as.character(wektorIDFilm1),
                              "i o IDFilm2=",as.character(wektorIDFilm3[i]),sep=" ")
            print(napis)
         } 
      }
   }


}
