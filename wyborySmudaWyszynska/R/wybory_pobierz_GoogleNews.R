#' Pobieranie informacji z portalu GoogleNews
#'
#' Funkcja \code{wybory_pobierz_GoogleNews} zlicza wystapienia nazwisk kandydatow
#' we wszystkich artykulach na stronie GoogleNews : http://news.google.pl/.
#'
#' @details
#' Artykuly na tym portalu naleza do roznych stron, a kazda z nich ma swoja
#' organizacje tekstu, dlatego ponizszy kod wyciaga caly kod HTML strony
#' z artykulem i zlicza ile razy wystepuja w nim nazwiska kandydatow.
#'
#' Pliki zostana zapisane we wskazanym miejscu na dysku w folderze GoogleNews/, a
#' nazwa pliku bedzie w formacie "GoogleNews(czas systemowy).txt".
#'
#' @usage wybory_pobierz_GoogleNews(sciezka_zapis=getwd())
#'
#' @param sciezka_zapis sciezka do folderu w ktorym maja zostac zapisane artykuly
#'
#' @return Zwraca invisible NULL oraz komunikat z informacja o wynikach sesji pobierania.
#'
#' @examples
#' wybory_pobierz_GoogleNews()
#'
#' @author Karolina Wyszynska
#'

wybory_pobierz_GoogleNews <- function(sciezka_zapis=getwd()){
  #Ustawiamy sciezke do zapisu
  if(stri_sub(sciezka_zapis,-1)!="/"){
     path <- paste0(sciezka_zapis,"/")
  } else {path <- sciezka_zapis}

  #Wybieramy klucze, po których będziemy szukać linków do artykułów
  slowa_klucze <- slownik_google
  klucz <- stri_paste(slowa_klucze, collapse="|")

  #Zczytujemy stronę z newsami i zbieramy linki
  suppressWarnings(Strona <- readLines("http://news.google.pl/"))
  Linki <- unlist(stri_match_all_regex(Strona, '(?<=url=").+?(?=" id=|" ssid)'))
  Linki <- Linki[which(!is.na(Linki))]

  #Wybieramy tylko te linki, które dotyczą wyborów
  dobre_artykuly <- unlist(stri_match_all_regex(Linki, klucz))
  dobre_artykuly <- which(!is.na(dobre_artykuly))
  Linki <- Linki[dobre_artykuly]

  #Poprawiamy klucze na tokeny, by nie rozróżniać odmian jednego nazwiska
  slowa_klucze <- slownik_google_tokeny #Propozycja tokenow
  klucz <- stri_paste(slowa_klucze, collapse="|")

  #Otwieramy strony z artykułami i zliczamy ilość występowania nazwiska kandydatów
  n <- length(Linki)
  artykuly_kandydaci <- vector("list", n)
  for(i in seq_len(n)){
    suppressWarnings(Artykul <- readLines(Linki[i]))
    kandydaci <- unlist(stri_match_all_regex(Artykul,klucz))
    artykuly_kandydaci[[i]] <- kandydaci[which(!is.na(kandydaci))]
  }

  wyniki <- table(unlist(artykuly_kandydaci))
  #Zmieniamy tokeny na pełne nazwiska
  tokeny <- names(wyniki)
  for(i in seq_along(tokeny)){
    if(tokeny[i]=="Dud") tokeny[i]="Duda"
    if(tokeny[i]=="Grodzk") tokeny[i]="Grodzka"
    if(tokeny[i]=="Nowick") tokeny[i]="Nowicka"
    if(tokeny[i]=="Słomk") tokeny[i]="Słomka"
    if(tokeny[i]=="Tanajn") tokeny[i]="Tanajno"
  }
  names(wyniki) <- tokeny

  #Zapisujemy nasz wynik do pliku
  wyniki1 <- stri_paste(tokeny,as.integer(wyniki),collapse=" ",sep=" ")
  name <- paste(path,"GoogleNews/GoogleNews",stri_replace_all_fixed(
    as.character(Sys.time()),":","-"),".txt",collapse="",sep="")
  if(!file.exists(stri_paste(path,"/GoogleNews"))){dir.create(
    stri_paste(path,"/GoogleNews"))}
  file.create(name)
  f <- file(name,"w")
  writeLines(stri_paste(tokeny,collapse=" "),f)
  writeLines(stri_paste(as.integer(wyniki),collapse=" "),f)
  close(f)

  print("Google zapisany")

  invisible(NULL)
}

