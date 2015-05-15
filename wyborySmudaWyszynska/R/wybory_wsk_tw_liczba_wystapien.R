#' Wskaznik liczby wystapien kandydata w tweetach
#'
#' Funkcja \code{wybory_wsk_tw_liczba_wystapien} zlicza liczbe wystapien kandydata w tweetach.
#'
#' @usage
#' \code{wybory_wsk_tw_liczba_wystapien(okres,wzor,sciezka_odczyt=getwd())}
#'
#' @param okres okres, za ktory jest liczony wskaznik; mozliwosci: "dzien", "tydzien", "miesiac"
#' @param wzor wzorzec do odczytu plikow odpowiadajacy danemu okresowi; mozliwosci: "2015-XX-XX", "2015-(X|XX)", "2015-XX"
#' @param sciezka_odczyt sciezka do folderu bazowego z wszystkimi plikami
#'
#' @details
#' Sposob obliczania wskaznika liczby wystapien kandydata w tweetach na Twitterze:
#'
#' Dla danego okresu dla kazdego tweeta zlicza, jaki kandydat w nim sie pojawil,
#' skleja wszystko do ramki danych, a nastepnie sumuje po kolumnach (kandydaci)
#' liczbe wystapien.
#'
#' @return Dla danego okresu zwraca wyliczony wskaznik  w postaci wektora nazwanego nazwiskami poszczegolnych kandydatow.
#'
#' @examples
#' wybory_wsk_tw_liczba_wystapien("dzien","2015-04-14")
#' wybory_wsk_tw_liczba_wystapien("tydzien","2015-14")
#' wybory_wsk_tw_liczba_wystapien("miesiac","2015-04")
#'
#' @author Piotr Smuda
#'

wybory_wsk_tw_liczba_wystapien<-function(okres,wzor,sciezka_odczyt=getwd()){

   #wczytujemy imiona kandydatów i ich tokeny
   kandydaci<-slownik_fb_kandydaci
   tokeny<-slownik_google_tokeny

   #zmieniamy scieżkę i sprawdzamy jakie mamy tam pliki
   if(stri_sub(sciezka_odczyt,-1)!="/"){
      sciezka_odczyt<-paste0(sciezka_odczyt,"/")
   }

   sciezka_tweety<-paste0(sciezka_odczyt,"Twitter/tweety/")
   pliki_tweety<-list.files(sciezka_tweety,full.names=TRUE)

   #wczytujemy wszystkie tweety z danego okresu do pliku
   tweety<-wsk_tw_pomocnicza(pliki_tweety,okres,wzor)

   #sprawdzamy po tokenach, ile razy kandydaci pojawili się w tweetach
   if(length(tweety)>0) { #gdy mamy jakieś tweety
      wskaznik<-lapply(tweety,stri_count_regex,tokeny)
      wskaznik<-do.call(rbind,wskaznik)
      wskaznik<-apply(wskaznik,2,sum)
   }
   else { #gdy nie mamy pasujących plików
         wskaznik<-numeric(length(kandydaci))
   }
   names(wskaznik)<-kandydaci
   return(wskaznik)
}

wsk_tw_wczytaj_json<-function(plik){
   #wczytujemy antysłownik dla tweetów
   antyslownik<-slownik_tw_anty

   #zamieniamy plik .json na ramkę danych
   sparsowane<-parseTweets(plik,simplify=FALSE,verbose=FALSE)

   #wybieram właściwe tweety (z pominięciem słów z antysłownika, który zawiera
   #słowa kluczowe związane z polską ligą piłki nożnej
   wlasciwe<-lapply(sparsowane$text,function(tweet){
         !stri_detect_regex(tweet,antyslownik)
      })
   wlasciwe<-sapply(wlasciwe, function(wektor){
         all(wektor==TRUE)
      })
   wlasciwe<-sparsowane$text[wlasciwe]
   #zwracam wektor z tweetami
   return(wlasciwe)
}

wsk_tw_pomocnicza<-function(pliki,okres,wzor){
   if(okres=="dzien"){
      #wybieramy pasujący do wzoru plik i go wczytujemy
      ktory<-which(stri_detect_regex(pliki,wzor))
      if(length(ktory)>0) {
         tweety<-wsk_tw_wczytaj_json(pliki[ktory])
      }
      else {
         tweety<-character()
      }
      return(tweety)
   }
   else if(okres=="tydzien"){
      #zapamietujemy interesujący nas numer tygodnia
      nr_tyg<-unlist(stri_extract_all_regex(wzor,"(?<=-).+$"))
      #wyciągamy z plików informację, który tydzień opisują
      daty<-unlist(stri_extract_all_regex(pliki,"(?<=tweety-).+(?=.json)"))
      daty<-as.POSIXlt(daty)
      tygodnie<-ceiling((daty$yday+4)/7)
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(tygodnie==nr_tyg)
      if(length(ktore)>0) { #gdy były pasujące, a jak nie to zwracamy character()
         tweety<-unlist(lapply(pliki[ktore],wsk_tw_wczytaj_json))
      }
      else {
         tweety<-character()
      }
      return(tweety)
   }
   else if(okres=="miesiac"){
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore<-which(stri_detect_regex(pliki,wzor))
      if(length(ktore)>0) { #gdy były pasujące, a jak nie to zwracamy character()
         tweety<-unlist(lapply(pliki[ktore],wsk_tw_wczytaj_json))
      }
      else {
         tweety<-character()
      }
      return(tweety)
   }
}