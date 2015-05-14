#' Pobieranie informacji z portalu WP
#'
#' Funkcja \code{wybory_pobierz_WP} pobiera artykuly z portalu WP,
#' a dokladniej ze strony dedykowanej dla wyborow:
#' "http://wiadomosci.wp.pl/kat,140394,title,Wybory-prezydenckie-w-2015-r,raport.html".
#'
#' @usage wybory_pobierz_WP(sciezka_zapis=getwd())
#'
#' @param sciezka_zapis sciezka do folderu w ktorym maja zostac zapisne artykuly
#'
#' @details
#' Korzystamy ze strony informacyjnej Interii z wyselekcjonowanymi
#' informacjami na temat wyborow. Artykuly, ktore sa tam podlinkowane maja podobna
#' strukture, dlatego wyciagamy z nich caly tekst artykulu i zapisujemy go
#' do pliku.
#'
#' Pliki zostana zapisane we wskazanym miejscu na dysku w folderze WirtualnaPolska/(czas systemowy)/, a
#' nazwa pliku bedzie w formacie liczb calkowitych od 1 do 18 z rozszerzeniem *.txt.
#'
#' @return Zwraca invisible NULL oraz komunikat z informacja o wynikach sesji pobierania.
#'
#' @examples
#' wybory_pobierz_WP()
#'
#' @author Karolina Wyszynska
#'

wybory_pobierz_WP <- function(sciezka_zapis=getwd()){
  #Ustawiamy sciezke do zapisu
  if(stri_sub(sciezka_zapis,-1)!="/"){
     path<-paste0(sciezka_zapis,"/")
  } else {path <- sciezka_zapis}


  #Pobieramy linki do artykułów
  wp <- "http://wiadomosci.wp.pl/kat,140394,title,Wybory-prezydenckie-w-2015-r,raport.html"
  Strona <- html(wp)
  StronaWezly <- html_nodes(Strona,"#bxRaportSpecjalny h2 a")
  StronaLinki <- html_attr(StronaWezly, "href")

 #Ściagamy teksty poszczególnych artykułów
 #Zapisujemy je w folderze z datą i godziną pobrania
 name <- paste(path, "WirtualnaPolska/", stri_replace_all_fixed(
         as.character(Sys.time()), ":", "-"), collapse="", sep="")
 if (!file.exists(stri_paste(path, "WirtualnaPolska"))){
   dir.create(stri_paste(path, "WirtualnaPolska"))}
 dir.create(name)

  for(i in seq_along(StronaLinki)){
    Strona <- html(stri_paste("http://wiadomosci.wp.pl", StronaLinki[i]))
    TytulArtykuluWezel <- html_nodes(Strona, "h1")
    TytulArtykulu <- html_text(TytulArtykuluWezel)
    TextWezly <- html_nodes(Strona, "#intertext1 , .lead")
    Text <- html_text(TextWezly)
    name1 <- paste(name, "/", i, ".txt",
               collapse="", sep="")
    file.create(name1)
    f <- file(name1, "w")
    writeLines(stri_paste(TytulArtykulu,"\n\n"), f)
    writeLines(Text, f)
    close(f)
 }
 print("WP zapisana")
 invisible(TRUE)
}

