#' Pobieranie postow z fanpage'y kandydatow
#'
#' Funkcja \code{wybory_pobierz_fb_posty} pobiera posty z fanpage'y kandydatow z dnia dzisiejszego.
#'
#' @usage
#' \code{wybory_pobierz_fb_posty(sciezka_zapis=getwd(),sciezka_dostep_api)}
#'
#' @param sciezka_zapis sciezka do miejsca na dysku, gdzie ma zostac zapisany plik z pobranymi postami
#' @param sciezka_dostep_api sciezka do pliku z pelna autoryzacja do aplikacji facebook
#'
#' @details
#' Funkcja dzieki dostepowi do aplikacji na facebooku wyciaga wszystkie dostepne informacje o postach
#' z fanpage'ow kandydatow.
#'
#' Pliki zostana zapisane we wskazanym miejscu na dysku w folderze Facebook/posty/(imie i nazwisko kandydata)/, a
#' nazwa pliku bedzie w formacie "posty-(data systemowa).txt".
#'
#' KROTKA INSTRUKCJA ZAPISU PLIKU Z AUTORYZACJA DO APLIKACJI NA FACEBOOKU
#'
#' 1. Na poczatku trzeba stworzyc aplikacje API na facebooku.
#'
#' 2. Potem tworzy sie zmienna srodowiskowa np. fb_oauth
#'    przy pomocy funkcji fbOAuth(app_id,app_secret,extended_permissions=TRUE).
#'
#' 3. Nastepnie zapisuje sie ja do pliku przy pomocy funkcji save(fb_oauth,file).
#'
#' @return Zwraca invisible NULL oraz komunikat z informacja o wynikach sesji pobierania.
#'
#' @author Piotr Smuda
#'

wybory_pobierz_fb_posty<-function(sciezka_zapis=getwd(),sciezka_dostep_api){
   #wczytujemy plik z dostepem do api
   load(sciezka_dostep_api)

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

   #ilu mamy kandydatów
   n<-length(strony_kandydatow)

   #tworzymy ścieżkę do folderu z postami kandydatów
   sciezka_posty<-paste0(sciezka,"posty/")
   dir.create(sciezka_posty,showWarnings=FALSE)

   #pętla do wychwycenia postów ze stron kandydatów i zapisania ich do plików
   for(i in seq_len(n))
   {
   #Jeśli stona jest w wektorze stron jako NA lub nie było postów w danym dniu, to
   #  przypisujemy mu wektor pusty -> w pliku widoczne to jako "x"
   posty<-tryCatch(getPage(page=id_kandydatow[i],token=fb_oauth,n=100,since=paste0(Sys.Date(),"T00:00:01"),
      until=paste0(Sys.Date(),"T23:59:59")),error=function(err) character(0))
   sciezka_posty_kandydat<-paste0(sciezka_posty,kandydaci[i])
   dir.create(sciezka_posty_kandydat,showWarnings=FALSE)
   write.table(posty,paste0(sciezka_posty_kandydat,"/posty-",Sys.Date(),".txt"))
   }

   return(invisible(NULL))
}

