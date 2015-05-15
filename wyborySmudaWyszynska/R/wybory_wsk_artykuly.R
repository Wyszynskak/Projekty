#' Wskaznik widocznosci  kandydata - portale internetowe
#'
#' Funkcja \code{wybory_wsk_artykuly} liczy wskaznik widocznosci kandydatow na portalach informacyjnych.
#'
#' @usage wybory_wsk_artykuly(okres, wzor, serwis, sciezka_do_folderu=getwd())
#'
#' @param sciezka_do_folderu sciezka do folderu, ktory zawiera podfolder z wynikami dzialania funkcji wybory_pobierz_WP(), wybory_pobierz_Interia() lub wybory_pobierz_Onet()
#' @param okres okres, za ktory jest liczony wskaznik; mozliwosci: "dzien", "tydzien", "miesiac"
#' @param wzor wzorzec do odczytu plikow odpowiadajacy danemu okresowi; mozliwosci: "2015-XX-XX", "2015-(X|XX)", "2015-XX"
#' @param serwis nazwa serwisu, dla ktorego obliczany ma byc wskaznik: "Interia", "Onet", "WP"
#'
#' @details Dla kazdego artykulu oraz kazdego kandydata przeprowadzamy analize tekstowa
#' zdan, w ktorych wystepuje nazwisko kandydata. (Gdy zdan nie odnaleziono
#' wskaznik wynosi 0). Sumujemy oceny slow razy ilosc ich wystepowania i dzielimy
#' wynik przez ilosc ocenionych slow. Dostajemy wskaznik z przedzialu [1,5]
#' (Gdy brak slow do oceny - wstawiamy wskaznik neutralny = 3)
#' Nastepnie przemnazamy to przez range artykulu (te wyzej na stronie sa bardziej
#' widoczne) i sumujemy po wszystkich artykulach.
#'
#' @return Dla danego okresu zwraca wyliczony wskaznik  w postaci wektora nazwanego nazwiskami poszczegolnych kandydatow.
#'
#' @examples
#' wybory_wsk_artykuly("dzien", "2015-03-23", "Interia")
#' wybory_wsk_artykuly("dzien", "2015-03-23", "WirtualnaPolska")
#' wybory_wsk_artykuly("miesiac", "2015-04", "Onet")
#'
#' @author Karolina Wyszynska
#'

wybory_wsk_artykuly <- function(okres, wzor, serwis, sciezka_do_folderu=getwd()){

   #Potrzebne wielkości
   kandydaci <- slownik_fb_kandydaci

   slownik <- slownik_wydzwieku
   slownik <- slownik[ ,c(1,5)]
   slownik[ ,1] <- stri_encode(slownik[ ,1], from="UTF8", to="cp1250")
   slownik[ ,2] <- slownik[ ,2] + 3

   tokeny <- slownik_google_tokeny

  #Zmieniamy scieżkę i sprawdzamy jakie mamy tam pliki
  if(stri_sub(sciezka_do_folderu,-1)!="/"){
     sciezka <- paste0(sciezka_do_folderu,"/")
  }
  sciezka <- paste0(sciezka, serwis)
  pliki <- list.dirs(sciezka, full.names=TRUE)
  pliki <- pliki[-1]

  ktore <- character(0)
  #Wybieramy pasujące do wzoru foldery
  if (okres == "dzien"){

    ktore <- which(stri_detect_regex(pliki, wzor))

  } else if (okres == "tydzien"){
      #zapamietujemy interesujący nas numer tygodnia
      nr_tyg <- unlist(stri_extract_all_regex(wzor, "(?<=-).+$"))
      #wyciągamy z plików informację, który tydzień opisują
      daty <- unlist(stri_extract_first_regex(list.dirs(sciezka,full.names=FALSE), "[^ ]+"))
      daty <- daty[-1]
      daty <- as.POSIXlt(daty)
      tygodnie <- ceiling((daty$yday + 4) / 7)
      #wybieramy pasujące do wzoru foldery
      ktore <- which(tygodnie == nr_tyg)
  } else if (okres == "miesiac"){
      #wybieramy pasujące do wzoru pliki i je wczytujemy
      ktore <- which(stri_detect_regex(pliki, wzor))
  }

  #Analizujemy interesujące nas foldery
  wskaznik <- numeric(length(kandydaci))
  names(wskaznik) <- kandydaci
  if(length(ktore) == 0){return(wskaznik)}

  for(i in seq_along(ktore)){
    wskaznik <- AnalizaArtykulyzFolderu(pliki[ktore[i]]) + wskaznik
  }

  wskaznik

}

 AnalizaArtykul <- function(sciezka.artykul){
    #Funkcja obliczajaca wskaźnik dla jednego artykułu

    #Potrzebne wielkości
    kandydaci <- slownik_fb_kandydaci

    slownik <- slownik_wydzwieku
    slownik <- slownik[ ,c(1,5)]
    slownik[ ,1] <- stri_encode(slownik[ ,1], from="UTF8", to="cp1250")
    slownik[ ,2] <- slownik[ ,2] + 3

    tokeny <- slownik_google_tokeny

    f <- file(sciezka.artykul, "r")
    text <- readLines(f)
    close(f)

    zdania <- stri_paste(text, collapse=" ") %>% unlist() %>%
       stri_split_boundaries(
          opts_brkiter=stri_opts_brkiter(type="sentence")) %>% unlist()

    wskaznik <- numeric(length(tokeny))

    #Dla kazdego kandydata
    for(i in seq_along(tokeny)){
       #Wyjmujemy zdania z jego nazwiskiem
       wykryj <- stri_detect(zdania, regex=tokeny[i])
       if (length(which(wykryj)) == 0){wskaznik[i] = 0;next}
       analiza <- paste(zdania[which(wykryj)], collapse=" ")
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

 AnalizaArtykulyzFolderu <- function(sciezka.folder){
    #Potrzebne wielkości
    kandydaci <- slownik_fb_kandydaci

    slownik <- slownik_wydzwieku
    slownik <- slownik[ ,c(1,5)]
    slownik[ ,1] <- stri_encode(slownik[ ,1], from="UTF8", to="cp1250")
    slownik[ ,2] <- slownik[ ,2] + 3

    tokeny <- slownik_google_tokeny

    pliki <- list.files(sciezka.folder, full.names=TRUE)
    n <- length(pliki)

    #Sumuje wskaźniki dla artykułów z odpowiednią dla nich rangą
    for(i in seq_along(pliki)){
       wskaznik.artykul <- AnalizaArtykul(pliki[i])
       waga <- stri_extract_last_regex(pliki[i], "[^//]{1,2}(?=[.]txt)") %>%
          unlist() %>% as.integer()
       ranga <- (n-waga+1)/n
       if(i==1){
          wskaznik <- wskaznik.artykul*ranga
       }else{
          wskaznik <- wskaznik + wskaznik.artykul*ranga}
    }

    wskaznik

 }

