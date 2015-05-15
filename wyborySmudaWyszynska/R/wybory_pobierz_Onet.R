#' Pobieranie informacji z portalu Onet
#'
#' Funkcja \code{wybory_pobierz_Onet} pobiera artykuly z portalu Onet,
#' a dokladniej ze strony dedykowanej dla wyborow:
#' "http://wiadomosci.onet.pl/wybory-prezydenckie-2015".
#'
#' @usage wybory_pobierz_Onet(sciezka_zapis=getwd())
#'
#' @param sciezka_zapis sciezka do folderu w ktorym maja zostac zapisne artykuly
#'
#' @details
#' Korzystamy ze strony informacyjnej Interii z wyselekcjonowanymi
#' informacjami na temat wyborow. Artykuly, ktore sa tam podlinkowane maja podobna
#' strukture, dlatego wyciagamy z nich caly tekst artykulu i zapisujemy go
#' do pliku.
#'
#' Pliki zostana zapisane we wskazanym miejscu na dysku w folderze Onet/(czas systemowy)/, a
#' nazwa pliku bedzie w formacie liczb calkowitych od 1 do 9 z rozszerzeniem *.txt.
#'
#' @return Zwraca invisible NULL oraz komunikat z informacja o wynikach sesji pobierania.
#'
#' @examples
#' wybory_pobierz_Onet()
#'
#' @author Karolina Wyszynska
#'

wybory_pobierz_Onet <- function(sciezka_zapis=getwd()){
  #Ustawiamy sciezke do zapisu
  if(stri_sub(sciezka_zapis,-1)!="/"){
     path<-paste0(sciezka_zapis,"/")
  } else {path <- sciezka_zapis}

  onet <- "http://wiadomosci.onet.pl/wybory-prezydenckie-2015"
  Strona <- readLines(onet)
  TytulyLinki <- unlist(stri_match_all_regex(
    Strona, '(?<=href="http://wiadomosci.onet.pl/).+?(?=" title=")'))
  TytulyLinki <- TytulyLinki[which(!is.na(TytulyLinki))]

  #Ściagamy teksty poszczególnych artykułów
  #Zapisujemy je w folderze z datą i godziną pobrania
  name <- paste(path, "Onet/", stri_replace_all_fixed(
    as.character(Sys.time()), ":", "-"), collapse="", sep="")
  if(!file.exists(stri_paste(path, "Onet"))){dir.create(stri_paste(path, "Onet"))}
  dir.create(name)

  for(i in seq_along(TytulyLinki)){
    Strona <- readLines(stri_paste("http://wiadomosci.onet.pl/", TytulyLinki[i]))
    TytulArtykulu <- unlist(stri_match_all_regex(
      Strona, "(?<=<title>).+?(?=</title>)"))
    TytulArtykulu <- TytulArtykulu[which(!is.na(TytulArtykulu))]
    Tresc <- unlist(stri_match_all_regex(
      Strona, '(?<=<p class=\"hyphenate\">).+?(?=</p>)'))
    Tresc <- Tresc[which(!is.na(Tresc))]
    name1 <- paste(name, "/", i, ".txt",
                 collapse="", sep="")
    file.create(name1)
    f <- file(name1,"w")
    writeLines(stri_paste(TytulArtykulu, "\n\n"), f)
    writeLines(Tresc, f)
    close(f)
  }


  print("Onet zapisany")

  invisible(NULL)
}
