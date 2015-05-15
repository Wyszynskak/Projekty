#' Pobieranie informacji z portalu Interia
#'
#' Funkcja \code{wybory_pobierz_Interia} pobiera artykuly z portalu Interia,
#' a dokladniej ze strony dedykowanej dla wyborow:
#' "http://fakty.interia.pl/raport-wybory-prezydenckie-2015".
#'
#' @usage wybory_pobierz_Interia(sciezka_zapis=getwd())
#'
#' @param sciezka_zapis sciezka do folderu w ktorym maja zostac zapisne artykuly
#'
#' @details
#' Korzystamy ze strony informacyjnej Interii z wyselekcjonowanymi
#' informacjami na temat wyborow. Artykuly, ktore sa tam podlinkowane maja podobna
#' strukture, dlatego wyciagamy z nich caly tekst artykulu i zapisujemy go
#' do pliku.
#'
#' Pliki zostana zapisane we wskazanym miejscu na dysku w folderze Interia/(czas systemowy)/, a
#' nazwa pliku bedzie w formacie liczb calkowitych od 1 do 10 z rozszerzeniem *.txt.
#'
#' @return Zwraca invisible NULL oraz komunikat z informacja o wynikach sesji pobierania.
#'
#' @examples
#' wybory_pobierz_Interia()
#'
#' @author Karolina Wyszynska
#'

wybory_pobierz_Interia <- function(sciezka_zapis=getwd()){
  #Ustawiamy sciezke do zapisu
  if(stri_sub(sciezka_zapis,-1)!="/"){
     path<-paste0(sciezka_zapis,"/")
  } else {path <- sciezka_zapis}
  
  #Pobieramy linki do artykułów
  interia <- "http://fakty.interia.pl/raport-wybory-prezydenckie-2015"
  Strona <- html(interia)
  StronaWezly <- html_nodes(Strona, "#brief_list_1 .brief-title-link")
  StronaLinki <- html_attr(StronaWezly, "href")

  #Ściagamy teksty poszczególnych artykułów
  #Zapisujemy je w folderze z datą i godziną pobrania
  name <- paste(path, "Interia/", stri_replace_all_fixed(
    as.character(Sys.time()), ":", "-"), collapse="", sep="")
  if (!file.exists(stri_paste(path,"Interia"))){
    dir.create(stri_paste(path, "Interia"))}
  dir.create(name)

  for(i in seq_along(StronaLinki)){
    Strona <- html(stri_paste("http://fakty.interia.pl", StronaLinki[i]))
    TytulArtykuluWezel <- html_nodes(Strona, "#articleSingle1 h1")
    TytulArtykulu <- html_text(TytulArtykuluWezel)
    TextWezly <- html_nodes(Strona, ".fontSize-medium , #articleSingle1 p")
    Text <- html_text(TextWezly)
    name1 <- paste(name, "/", i, ".txt",
                 collapse="", sep="")
    file.create(name1)
    f <- file(name1,"w")
    writeLines(stri_paste(TytulArtykulu, "\n\n"), f)
    writeLines(Text, f)
    close(f)
  }

  print("Interia zapisana")
  invisible(NULL)
}


