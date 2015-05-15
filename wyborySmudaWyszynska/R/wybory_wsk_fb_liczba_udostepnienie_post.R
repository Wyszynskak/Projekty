#' Wskaznik sredniej liczby udostepnien na posta na fanpage'u kandydata
#'
#' Funkcja \code{wybory_wsk_fb_liczba_udostepnienie_post} zlicza srednia liczbe udostepnien na posta na fanpage'u kandydata.
#'
#' @usage
#' \code{wybory_wsk_fb_liczba_udostepnienie_post(okres,wzor,sciezka_odczyt=getwd())}
#'
#' @param okres okres, za ktory jest liczony wskaznik; mozliwosci: "dzien", "tydzien", "miesiac"
#' @param wzor wzor do odczytu plikow odpowiadajacy danemu okresowi; mozliwosci: "2015-XX-XX", "2015-(X|XX)", "2015-XX"
#' @param sciezka_odczyt sciezka do folderu bazowego z wszystkimi plikami
#'
#' @details
#' Sposob obliczania wskaznika "sredniej ilosci" udostepnien postow na fanpage'u:
#'
#' Dla danego okresu zlicza srednia arytmetyczna liczby udostepnien postow
#' opublikowanych przez kandydata.
#'
#' @return Dla danego okresu zwraca wyliczony wskaznik  w postaci wektora nazwanego nazwiskami poszczegolnych kandydatow.
#'
#' @examples
#' wybory_wsk_fb_liczba_udostepnienie_post("dzien","2015-04-14")
#' wybory_wsk_fb_liczba_udostepnienie_post("tydzien","2015-14")
#' wybory_wsk_fb_liczba_udostepnienie_post("miesiac","2015-04")
#'
#' @author Piotr Smuda
#'

wybory_wsk_fb_liczba_udostepnienie_post<-function(okres,wzor,sciezka_odczyt=getwd()){

   #wczytujemy imiona kandydatów
   kandydaci<-slownik_fb_kandydaci

   #zmieniamy scieżkę
   if(stri_sub(sciezka_odczyt,-1)!="/"){
      sciezka_odczyt<-paste0(sciezka_odczyt,"/")
   }

   sciezka_fb_posty<-paste0(sciezka_odczyt,"Facebook/posty/")
   sciezki_fb_posty<-paste0(sciezka_fb_posty,kandydaci)
   pliki_fb_posty<-lapply(sciezki_fb_posty,list.files,full.names=TRUE)

   #wywołujemy funkcję pomocniczą
   posty_kandydaci<-lapply(pliki_fb_posty,wsk_fb_posty_pomocnicza,okres,wzor)

   #zliczamy średnią zdobytych polubień do posta dla kandydatów
   wskaznik<-sapply(posty_kandydaci,function(ramka){
      if(!is.null(ramka)) { #jeśli były posty, to zliczamy, wpp. dajemy 0
         liczba_udostepnienie_post<-mean(ramka[,3])
         liczba_udostepnienie_post<-round(liczba_udostepnienie_post,2)
      }
      else {
         liczba_udostepnienie_post<-0
      }
      })
   names(wskaznik)<-kandydaci
   return(wskaznik)
}