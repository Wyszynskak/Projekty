#' Pobieranie liczby polubien z fanpage'y kandydatow
#'
#' Funkcja \code{wybory_pobierz_fb_likes} pobiera aktualna liczbe polubien z fanpage'y kandydatow.
#'
#' @usage
#' \code{wybory_pobierz_fb_likes(sciezka_zapis=getwd())}
#'
#' @param sciezka_zapis sciezka do miejsca na dysku, gdzie ma zostac zapisany plik z pobranymi polubieniami
#'
#' @details
#' Funkcja wyciaga z kodu zrodlowego fanpage'ow kandydatow liczbe polubien.
#'
#' Pliki zostana zapisane we wskazanym miejscu na dysku w folderze Facebook/likes/, a
#' nazwa pliku bedzie w formacie "facebook_likes-(czas systemowy).txt".
#'
#' @return Zwraca invisible NULL.
#'
#' @examples
#' wybory_pobierz_fb_likes()
#'
#' @author Piotr Smuda
#'

wybory_pobierz_fb_likes<-function(sciezka_zapis=getwd()){

   #wczytujemy jeszcze dane nam potrzebne
   strony_kandydatow<-slownik_fb_strony
   kandydaci<-slownik_fb_kandydaci

   #potrzebny wektor z unikalnymi id do pobrania danych ze strony z fb
   id_kandydatow<-unlist(stri_extract_all_regex(strony_kandydatow,"[^/]+?$"))

   if(stri_sub(sciezka_zapis,-1)!="/"){
      sciezka_zapis<-paste0(sciezka_zapis,"/")
   }

   #zmieniamy scieżkę, na tą gdzie pliki będziemy zapisywać
   sciezka<-paste0(sciezka_zapis,"Facebook/")
   dir.create(sciezka,showWarnings=FALSE)

   #ilu mamy kandydatów + stworzenie wektora numerycznego dla liczby polubień
   n<-length(strony_kandydatow)
   lajki_fanpagow<-numeric(n)

   #pętla do wychwycenia liczby polubień ze stron kandydatów
   for(i in seq_len(n))
   {
      #pomijamy kandydatów, którzy nie mają strony z liczbą polubień i dajemy im wartość 0
      if(!(strony_kandydatow[i]=="NA") && i!=12 && i!=14 && i!=16 && i!=21 && i!=24)
      {
         strona<-readLines(strony_kandydatow[i],warn=FALSE)
         lajki<-stri_extract_all_regex(strona,"(?<=Polubienia:</span>).+?(?=</span>)",simplify=TRUE)
         lajki<-lajki[which(!is.na(lajki))]
         lajki_fanpagow[i]<-as.numeric(stri_flatten(stri_extract_all_regex(lajki,"[0-9]+",simplify=TRUE)))
      }
      else
      {
         lajki_fanpagow[i]<-0
      }
   }

   #tworzymy folder, a następnie plik z wektorem liczby polubień
   names(lajki_fanpagow)<-kandydaci
   sciezka_lajki<-paste0(sciezka,"likes/")
   dir.create(sciezka_lajki,showWarnings=FALSE)
   adres<-paste(sciezka_lajki,"facebook_likes-",stri_replace_all_fixed(
      as.character(Sys.time()),":","-"),".txt",collapse="",sep="")
   file.create(adres)
   f<-file(adres,"w")
   writeLines(stri_paste(kandydaci,collapse=";"),f) #otwieranie pliku przy pomocy read.csv2
   writeLines(stri_paste(lajki_fanpagow,collapse=";"),f)
   close(f)

   return(invisible(NULL))
}
