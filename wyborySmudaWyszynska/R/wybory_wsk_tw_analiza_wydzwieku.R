#' Wskaznik wydzwieku tweetow z kandydatami
#'
#' Funkcja \code{wybory_wsk_tw_analiza_wydzwieku} zlicza wydzwiek tweetow z kandydatami.
#'
#' @usage
#' \code{wybory_wsk_tw_analiza_wydzwieku(okres,wzor,sciezka_odczyt=getwd())}
#'
#' @param okres okres, za ktory jest liczony wskaznik; mozliwosci: "dzien", "tydzien", "miesiac"
#' @param wzor wzor do odczytu plikow odpowiadajacy danemu okresowi; mozliwosci: "2015-XX-XX", "2015-(X|XX)", "2015-XX"
#' @param sciezka_odczyt sciezka do folderu bazowego z wszystkimi plikami
#'
#' @details
#' Sposob obliczania wskaznika wydzwieku tweetow o kandydatach na Twitterze:
#'
#' Opisuje wydzwiekk tweetow zebranych w danym okresie, w ktorych znaleziono nazwisko
#' danego kandydata. Dla konkretnego kandydata wyszukujemy wszystkie tweety z jego
#' nazwiskiem i oceniamy wartosc poszczegolnych slow wystepujacych w tych tweetach.
#' Sumujemy te wartosci. By uzyskac wartosc z przedzialu [1, 5] dzielimy to przez
#' ilosc ocenionych wyrazow. Jesli nie wykryto, zadnego zabarwionego emocjonalnie
#' slowa tweet uznajemy za neutralny i otrzymuje wartosc 3. Gdy nie znalezlismy
#' zadnych tweetow dotyczacych danego kandydata przyjmujemy wskaznik rowny 0.
#'
#' @return Dla danego okresu zwraca wyliczony wskaznik  w postaci wektora nazwanego nazwiskami poszczegolnych kandydatow.
#'
#' @examples
#' wybory_wsk_tw_analiza_wydzwieku("dzien","2015-04-14")
#' wybory_wsk_tw_analiza_wydzwieku("tydzien","2015-14")
#' wybory_wsk_tw_analiza_wydzwieku("miesiac","2015-04")
#'
#' @author Karolina Wyszynska
#'

wybory_wsk_tw_analiza_wydzwieku<-function(okres,wzor,sciezka_odczyt=getwd()){

   #wczytujemy imiona kandydatów i ich tokeny
   kandydaci<-slownik_fb_kandydaci
   tokeny<-slownik_google_tokeny

   #wczytujemy słownik wydźwięku dla tweetów
   slownik <- slownik_wydzwieku
   slownik <- slownik[ ,c(1,5)]
   slownik[ ,1] <- stri_encode(slownik[ ,1], from="UTF8", to="cp1250")
   slownik[ ,2] <- slownik[ ,2] + 3

   #zmieniamy scieżkę
   if(stri_sub(sciezka_odczyt,-1)!="/"){
      sciezka_odczyt<-paste0(sciezka_odczyt,"/")
   }
   sciezka_tweety<-paste0(sciezka_odczyt,"Twitter/tweety/")
   pliki_tweety<-list.files(sciezka_tweety,full.names=TRUE)

   tweety <- wsk_tw_pomocnicza(pliki_tweety, okres, wzor)
   wskaznik <- numeric(length(tokeny))

   #Dla kazdego kandydata
   for(i in seq_along(tokeny)){
      #Wyjmujemy zdania z jego nazwiskiem
      wykryj <- stri_detect(tweety, regex=tokeny[i])
      if (length(which(wykryj)) == 0){wskaznik[i] = 0;next}
      analiza <- paste(tweety[which(wykryj)], collapse=" ")
      #Zliczamy występujące w nich słowa i robimy ich analizę
      analiza <- stri_extract_all_words(analiza) %>% unlist() %>% table()
      wspolne <- intersect(slownik[ ,1], names(analiza))
      if(length(wspolne)==0){wskaznik[i] = 3;next}

      #Obliczamy wskaźnik
      wskaznik[i] <- sum(analiza[wspolne] * slownik[slownik[ ,1] %in% wspolne,
         2]) / sum(analiza[wspolne])
   }

   names(wskaznik) <- kandydaci
   wskaznik
}