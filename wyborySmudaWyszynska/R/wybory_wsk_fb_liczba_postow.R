#' Wskaznik ilosci postow na fanpage'u kandydata
#'
#' Funkcja \code{wybory_wsk_fb_liczba_postow} zlicza ilosci postow na fanpage'u kandydata.
#'
#' @usage
#' \code{wybory_wsk_fb_liczba_postow(okres,wzor,sciezka_odczyt=getwd())}
#'
#' @param okres okres, za ktory jest liczony wskaznik; mozliwosci: "dzien", "tydzien", "miesiac"
#' @param wzor wzor do odczytu plikow odpowiadajacy danemu okresowi; mozliwosci: "2015-XX-XX", "2015-(X|XX)", "2015-XX"
#' @param sciezka_odczyt sciezka do folderu bazowego z wszystkimi plikami
#'
#' @details
#' Sposob obliczania wskaznika liczby postow na fanpage'u:
#'
#' Dla danego okresu zlicza liczbe postow opublikowanych przez kandydata.
#'
#' @return Dla danego okresu zwraca wyliczony wskaznik  w postaci wektora nazwanego nazwiskami poszczegolnych kandydatow.
#'
#' @examples
#' wybory_wsk_fb_liczba_postow("dzien","2015-04-14")
#' wybory_wsk_fb_liczba_postow("tydzien","2015-14")
#' wybory_wsk_fb_liczba_postow("miesiac","2015-04")
#'
#' @author Piotr Smuda
#'

#Wskaźnik dla liczby postów na stronach na facebooku
wybory_wsk_fb_liczba_postow<-function(okres,wzor,sciezka_odczyt=getwd()){

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

   #zliczamy liczby postów dla kandydatów
   wskaznik<-sapply(posty_kandydaci,function(ramka){
      if(!is.null(ramka)) { #jeśli były posty, to zliczamy, wpp. dajemy 0
         liczba_postow<-nrow(ramka)
      }
      else {
         liczba_postow<-0
      }
      })
   names(wskaznik)<-kandydaci
   return(wskaznik)
}

wsk_fb_posty_pomocnicza<-function(lista,okres,wzor){
   if(okres=="dzien"){
      #wybieramy pasujący do wzoru plik i go wczytujemy
      ktory<-which(stri_detect_regex(lista,wzor))
      if(length(ktory)>0) {
         posty<-read.table(lista[ktory])
         #sprawdzamy, czy w danym dniu były opublikowane posty
         if(ncol(posty)==1){ #jeśli nie, to zwracamy NULL
            return(NULL)
         }
         #interesują nas tylko liczby polubień, komentarzy i udostępnień
         ramka<-posty[,8:10]
      }
      else {
         ramka<-NULL
      }
      return(ramka)
   }
   else if(okres=="tydzien"){
      #zapamietujemy interesujący nas numer tygodnia
      nr_tyg<-unlist(stri_extract_all_regex(wzor,"(?<=-).+$"))
      #wyciągamy z plików informację, który tydzień opisują
      daty<-unlist(stri_extract_all_regex(lista,"(?<=posty-).+(?=.txt)"))
      daty<-as.POSIXlt(daty)
      tygodnie<-ceiling((daty$yday+4)/7)
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(tygodnie==nr_tyg)
      if(length(ktore)>0) { #gdy były pasujące, a jak nie to zwracamy NULL
         posty<-lapply(lista[ktore],read.table)
         #sprawdzamy, w które dni były opublikowane posty
         niepuste<-sapply(posty,function(element){
            if(ncol(element)==1) {
               element<-FALSE
            }
            else {
               element<-TRUE
            }})
         #interesują nas pliki z postami
         posty<-posty[niepuste]
         #zapisujemy co całości
         ramka<-do.call(rbind,posty)
         #interesują nas tylko liczby polubień, komentarzy i udostępnień
         ramka<-ramka[,8:10]
      }
      else {
         ramka<-NULL
      }
      return(ramka)
   }
   else if(okres=="miesiac"){
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(stri_detect_regex(lista,wzor))
      if(length(ktore)>0) { #gdy były pasujące, a jak nie to zwracamy NULL
         posty<-lapply(lista[ktore],read.table)
         #sprawdzamy, w które dni były opublikowane posty
         niepuste<-sapply(posty,function(element){
            if(ncol(element)==1) {
               element<-FALSE
            }
            else {
               element<-TRUE
            }})
         #interesują nas pliki z postami
         posty<-posty[niepuste]
         #zapisujemy co całości
         ramka<-do.call(rbind,posty)
         #interesują nas tylko liczby polubień, komentarzy i udostępnień
         ramka<-ramka[,8:10]
      }
      else {
         ramka<-NULL
      }
      return(ramka)
   }
}