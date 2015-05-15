#' Wskaznik ilosci polubien na fanpage'u kandydata
#'
#' Funkcja \code{wybory_wsk_fb_likes} zlicza maksimum ilosci polubien na fanpage'u kandydata.
#'
#' @usage
#' \code{wybory_wsk_fb_likes(okres,wzor,sciezka_odczyt=getwd())}
#'
#' @param okres okres, za ktory jest liczony wskaznik; mozliwosci: "dzien", "tydzien", "miesiac"
#' @param wzor wzor do odczytu plikow odpowiadajacy danemu okresowi; mozliwosci: "2015-XX-XX", "2015-(X|XX)", "2015-XX"
#' @param sciezka_odczyt sciezka do folderu bazowego z wszystkimi plikami
#'
#' @details
#' Sposob obliczania wskaznika liczby polubien na fanpage'u:
#'
#' Dla danego okresu wybiera maksimum z liczby polubien.
#'
#' @return Dla danego okresu zwraca wyliczony wskaznik  w postaci wektora nazwanego nazwiskami poszczegolnych kandydatow.
#'
#' @examples
#' wybory_wsk_fb_likes("dzien","2015-04-14")
#' wybory_wsk_fb_likes("tydzien","2015-14")
#' wybory_wsk_fb_likes("miesiac","2015-04")
#'
#' @author Piotr Smuda
#'

wybory_wsk_fb_likes<-function(okres,wzor,sciezka_odczyt=getwd()){

   #wczytujemy imiona kandydatów
   kandydaci<-slownik_fb_kandydaci

   #zmieniamy scieżkę i sprawdzamy jakie mamy tam pliki
   if(stri_sub(sciezka_odczyt,-1)!="/"){
      sciezka_odczyt<-paste0(sciezka_odczyt,"/")
   }

   sciezka_fb_likes<-paste0(sciezka_odczyt,"Facebook/likes/")
   pliki_fb_likes<-list.files(sciezka_fb_likes,full.names=TRUE)

   if(okres=="dzien"){
      #wybieramy pasujący do wzoru plik i go wczytujemy
      ktory<-which(stri_detect_regex(pliki_fb_likes,wzor))
      if(length(ktory)>0) {
         wskaznik<-read.csv2(pliki_fb_likes[ktory])
      }
      else { #gdy nie mamy pasujących plików
         wskaznik<-numeric(length(kandydaci))
      }
      names(wskaznik)<-kandydaci
      #chcemy wektor na wyjściu
      wskaznik<-unlist(wskaznik)
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
         #interesuje nas maksymalny pomiar dla danego tygodnia
         wskaznik<-do.call(rbind,lapply(pliki_fb_likes[ktore],read.csv2))
         wskaznik<-apply(wskaznik,2,max)
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
         #interesuje nas maksymalny pomiar dla danego miesiąca
         wskaznik<-do.call(rbind,lapply(pliki_fb_likes[ktore],read.csv2))
         wskaznik<-apply(wskaznik,2,max)
      }
      else { #gdy nie mamy pasujących plików
         wskaznik<-numeric(length(kandydaci))
      }
      names(wskaznik)<-kandydaci
      return(wskaznik)
   }
}