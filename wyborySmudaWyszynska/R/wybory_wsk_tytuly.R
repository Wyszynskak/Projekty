#' Wskaznik widocznosci kandydata - portale internetowe - tytuly
#'
#' Funkcja \code{wybory_wsk_tytuly} zlicza widocznosc kandydatow w tytulach na portalach informacyjnych.
#'
#' @usage
#' wybory_wsk_tytuly(okres, wzor, serwis, sciezka_do_folderu=getwd())
#'
#' @param sciezka_do_folderu sciezka do folderu, ktory zawiera podfolder z wynikami dzialania funkcji wybory_pobierz_WP(), wybory_pobierz_Interia() lub wybory_pobierz_Onet()
#' @param okres okres, za ktory jest liczony wskaznik; mozliwosci: "dzien", "tydzien", "miesiac"
#' @param wzor wzorzec do odczytu plikow odpowiadajacy danemu okresowi; mozliwosci: "2015-XX-XX", "2015-(X|XX)", "2015-XX"
#' @param serwis nazwa serwisu, dla ktorego obliczany ma byc wskaznik: "Interia", "Onet", "WP"
#'
#' @details
#' Po podaniu portalu okresu, dla ktorego chcemy obliczyc wskaznik, oraz wzorca
#' okresu funkcja zczytuje pliki z danymi dotyczacymi tego okresu i analizuje
#' poszczegolne tytulu artykulow.
#'
#' Sposob obliczania wskaznika:
#'
#' Zliczana jest ilosc wystapien w tytulach artykulow i mnozona przez wage artykulu.
#'
#' @return Dla danego okresu zwraca wyliczony wskaznik  w postaci wektora nazwanego nazwiskami poszczegolnych kandydatow.
#'
#' @examples
#' wybory_wsk_tytuly("dzien", "2015-03-23", "Interia")
#' wybory_wsk_tytuly("dzien", "2015-03-23", "WirtualnaPolska")
#' wybory_wsk_tytuly("miesiac", "2015-04", "Onet")
#'
#' @author Karolina Wyszynska
#'



wybory_wsk_tytuly <- function(okres, wzor, serwis, sciezka_do_folderu=getwd()){

  #Potrzebne wielkosci
  kandydaci <- slownik_fb_kandydaci
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
    wskaznik <- AnalizaTytulowzFolderu(pliki[ktore[i]]) + wskaznik
  }

  wskaznik

}

#Funkcje pomocnicze
AnalizaTytul <- function(sciezka.artykul){
   #Potrzebne wielkosci
   kandydaci <- slownik_fb_kandydaci
   tokeny <- slownik_google_tokeny
   #Funkcja obliczajaca wskaźnik dla jednego tytułu

   f <- file(sciezka.artykul, "r")
   text <- readLines(f, n=2)
   close(f)

   slowa <- stri_paste(text, collapse=" ") %>% unlist() %>%
      stri_split_boundaries(
         opts_brkiter=stri_opts_brkiter(type="word")) %>% unlist()

   wskaznik <- numeric(length(tokeny))

   #Dla kazdego kandydata
   for(i in seq_along(tokeny)){
      #Zliczamy ilosc wystapien nazwiska
      wykryj <- stri_detect(slowa, regex=tokeny[i])
      wskaznik[i] <- sum(wykryj)
   }

   names(wskaznik) <- kandydaci
   wskaznik

}

AnalizaTytulowzFolderu <- function(sciezka.folder){
   pliki <- list.files(sciezka.folder, full.names=TRUE)
   n <- length(pliki)

   #Sumuje wskaźniki dla artykułów z odpowiednią dla nich rangą
   for(i in seq_along(pliki)){
      wskaznik.artykul <- AnalizaTytul(pliki[i])
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
