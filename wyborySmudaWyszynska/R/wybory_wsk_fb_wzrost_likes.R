#' Wskaznik sredniego wzrostu polubien na fanpage'u kandydata
#'
#' Funkcja \code{wybory_wsk_fb_wzrost_likes} zlicza sredni wzrost polubien na fanpage'u kandydata.
#'
#' @usage
#' \code{wybory_wsk_fb_wzrost_likes(okres,wzor,sciezka_odczyt=getwd())}
#'
#' @param okres okres, za ktory jest liczony wskaznik; mozliwosci: "dzien", "tydzien", "miesiac"
#' @param wzor wzor do odczytu plikow odpowiadajacy danemu okresowi; mozliwosci: "2015-XX-XX", "2015-(X|XX)", "2015-XX"
#' @param sciezka_odczyt sciezka do folderu bazowego z wszystkimi plikami
#'
#' @details
#' Sposob obliczania wskaznika "sredniego przyrostu" polubien na fanpage'u:
#'
#' Dla danego okresu zlicza zmiany liczby polubien fanpage'y, a nastepnie
#' zwraca ich srednia arytmetyczna. Dla dnia zwraca wartosc zerowa.
#'
#' @return Dla danego okresu zwraca wyliczony wskaznik  w postaci wektora nazwanego nazwiskami poszczegolnych kandydatow.
#'
#' @examples
#' wybory_wsk_fb_wzrost_likes("dzien","2015-04-14")
#' wybory_wsk_fb_wzrost_likes("tydzien","2015-14")
#' wybory_wsk_fb_wzrost_likes("miesiac","2015-04")
#'
#' @author Piotr Smuda
#'

wybory_wsk_fb_wzrost_likes<-function(okres,wzor,sciezka_odczyt=getwd()){

   #wczytujemy imiona kandydatów
   kandydaci<-slownik_fb_kandydaci

   #zmieniamy scieżkę i sprawdzamy jakie mamy tam pliki
   if(stri_sub(sciezka_odczyt,-1)!="/"){
      sciezka_odczyt<-paste0(sciezka_odczyt,"/")
   }

   sciezka_fb_likes<-paste0(sciezka_odczyt,"Facebook/likes/")
   pliki_fb_likes<-list.files(sciezka_fb_likes,full.names=TRUE)

   if(okres=="dzien"){
      wskaznik<-numeric(length(kandydaci))
      names(wskaznik)<-kandydaci
      return(wskaznik)
   }
   else if(okres=="tydzien"){
      #zapamietujemy interesujący nas numer tygodnia
      nr_tyg<-unlist(stri_extract_all_regex(wzor,"(?<=-).+$"))
      #wyciągamy z plików informację, który tydzień opisują
      daty<-unlist(stri_extract_all_regex(pliki_fb_likes,"(?<=likes-).+(?=.txt)"))
      daty<-as.POSIXlt(daty)
      tygodnie<-ceiling((daty$yday+4)/7)
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(tygodnie==nr_tyg)
      if(length(ktore)>0) {
         #interesuje nas średni przyrost w pomiarach dla danego tygodnia
         wskaznik<-do.call(rbind,lapply(pliki_fb_likes[ktore],read.csv2))
         wskaznik<-apply(wskaznik,2,diff)
         if(is.matrix(wskaznik)){
            wskaznik<-apply(wskaznik,2,mean)
         }
         wskaznik<-round(wskaznik,2)
      }
      else { #gdy nie mamy pasujących plików
         wskaznik<-numeric(length(kandydaci))
      }
      names(wskaznik)<-kandydaci
      return(wskaznik)
   }
   else if(okres=="miesiac"){
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(stri_detect_regex(pliki_fb_likes,wzor))
      if(length(ktore)>0) {
         #interesuje nas średni przyrost w pomiarach dla danego miesiąca
         wskaznik<-do.call(rbind,lapply(pliki_fb_likes[ktore],read.csv2))
         wskaznik<-apply(wskaznik,2,diff)
         if(is.matrix(wskaznik)){
            wskaznik<-apply(wskaznik,2,mean)
         }
         wskaznik<-round(wskaznik,2)
      }
      else { #gdy nie mamy pasujących plików
         wskaznik<-numeric(length(kandydaci))
      }
      names(wskaznik)<-kandydaci
      return(wskaznik)
   }
}