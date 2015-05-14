#' Wskazniki widocznosci kandydata w GoogleNews
#'
#' Funkcja \code{wybory_wsk_google} zlicza wskazniki
#' widocznosci kandydatow na portalu informacyjnym GoogleNews - wiecej patrz
#' funkcja wybory_pobierz_GoogleNews().
#'
#' @usage  wybory_wsk_GoogleNews(okres, wzor, typ="zwykly", sciezka_do_folderu=getwd())
#'
#' @param sciezka_do_folderu sciezka do folderu, ktory zawiera podfolder GoogleNews (wynik dzialania funkcji wybory_pobierz_GoogleNews())
#' @param okres okres, za ktory jest liczony wskaznik; mozliwosci: "dzien", "tydzien", "miesiac"
#' @param wzor wzorzec do odczytu plikow odpowiadajacy danemu okresowi; mozliwosci: "2015-XX-XX", "2015-(X|XX)", "2015-XX"
#' @param typ okresla typ wskaznika: "zwykly", "sredni"
#'
#' @details
#' Sposob obliczania wskaznika "sredniego" wyswietlenia na Google:
#'
#' Zwraca sume wyswietlen danego nazwiska na portalu dla danego okresu czasu.
#'
#' @return Dla danego okresu zwraca wyliczony wskaznik  w postaci wektora nazwanego nazwiskami poszczegolnych kandydatow.
#'
#' @examples
#' wybory_wsk_GoogleNews("dzien", "2015-03-23")
#' wybory_wsk_GoogleNews("tydzien", "2015-12")
#' wybory_wsk_GoogleNews("miesiac", "2015-03")
#' wybory_wsk_GoogleNews("dzien", "2015-03-23", "sredni")
#' wybory_wsk_GoogleNews("tydzien", "2015-12", "sredni")
#' wybory_wsk_GoogleNews("miesiac", "2015-04", "sredni")
#'
#' @author Karolina Wyszynska
#'

wybory_wsk_GoogleNews <- function(okres, wzor, typ="zwykly",
                                  sciezka_do_folderu=getwd()){

   #Zmieniamy scieżkę i sprawdzamy jakie mamy tam pliki
   if(stri_sub(sciezka_do_folderu,-1)!="/"){
      sciezka <- paste0(sciezka_do_folderu,"/GoogleNews")
   } else{
      sciezka <- paste0(sciezka_do_folderu, "GoogleNews")
   }

   pliki <- list.files(sciezka, full.names=TRUE)

   ktore <- character(0)
   #Wybieramy pasujące do wzoru foldery
   if (okres == "dzien"){
      ktore <- which(stri_detect_regex(pliki, wzor))
   } else if (okres == "tydzien"){
      #zapamietujemy interesujący nas numer tygodnia
      nr_tyg <- unlist(stri_extract_all_regex(wzor, "(?<=-).+$"))
      #wyciągamy z plików informację, który tydzień opisują
      daty <- unlist(stri_extract_first_regex(
         list.files(sciezka,full.names=FALSE), "(?<=News)[^ ]+"))
      daty <- as.POSIXlt(daty)
      tygodnie <- ceiling((daty$yday + 4) / 7)
      #wybieramy pasujące do wzoru foldery
      ktore <- which(tygodnie == nr_tyg)
   } else if (okres == "miesiac"){
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore <- which(stri_detect_regex(pliki, wzor))
   }

   #Analizujemy interesujące nas pliki
   wskaznik <- numeric(length(slownik_fb_kandydaci))
   names(wskaznik) <- slownik_fb_kandydaci
   if(length(ktore) == 0){return(wskaznik)}

   for(i in seq_along(ktore)){
      wskaznik <- AnalizaPlik(pliki[ktore[i]]) + wskaznik
   }

   if(typ == "sredni"){
      suma <- sum(wskaznik)
      wskaznik <- wskaznik/suma
   }

   wskaznik

}


#Funkcja pomocnicza
AnalizaPlik <- function(sciezka.plik){

   #Funkcja obliczajaca wskaźnik dla jednego czasu odczytu zliczeń z serwisu Google
   wystapienia <- character(0)
   try(wystapienia <- read.table(sciezka.plik,h=TRUE,encoding="UTF-8"), silent=TRUE)

   wskaznik <- numeric(length(slownik_fb_kandydaci))
   names(wskaznik) <- slownik_fb_kandydaci
   if(length(wystapienia)==0) return(wskaznik)

   #Poprawa struktury tabeli by spelniala wymagania wyjsciowego wektora
   imiona <- names(wystapienia)

   for(i in 1:ncol(wystapienia)){
      ktory <- which(stri_detect_regex(slownik_fb_kandydaci, imiona[i]))
      wskaznik[ktory] <- wystapienia[[i]]
   }

   wskaznik
}
