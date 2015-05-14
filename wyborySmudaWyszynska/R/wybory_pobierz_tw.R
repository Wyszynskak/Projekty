#' Pobieranie tweetow o kandydatach
#'
#' Funkcja \code{wybory_pobierz_tw} pobiera tweety ze slowami ze slownik_tw w czasie rzeczywistym.
#'
#' @usage
#' \code{wybory_pobierz_tw(sciezka_zapis=getwd(),sciezka_dostep_api,czas)}
#'
#' @param sciezka_zapis sciezka do miejsca na dysku, gdzie ma zostac zapisany plik z pobranymi tweetami
#' @param sciezka_dostep_api sciezka do pliku z pelna autoryzacja do aplikacji twitter
#' @param czas ilosc godzin dzialania funkcji
#'
#' @details
#' Funkcja dzieki dostepowi do aplikacji na twitterze pobiera w czasie rzeczywistym pojawiajace sie tweety.
#'
#' Pliki zostana zapisane we wskazanym miejscu na dysku w folderze Twitter/tweety/, a
#' nazwa pliku bedzie w formacie "tweety-(data systemowa).json".
#'
#' KROTKA INSTRUKCJA ZAPISU PLIKU Z AUTORYZACJA DO APLIKACJI NA TWITTERZE
#'
#' 1. Na poczatku trzeba stworzyc aplikacje API na Twitterze.
#'
#' 2. Potem tworzy sie jakas zmienna srodowiskowa np. paczka
#'   przy pomocy funkcji kodu:
#'
#' requestURL<-"https://api.twitter.com/oauth/request_token"
#'
#' accessURL<-"https://api.twitter.com/oauth/access_token"
#'
#' authURL<-"https://api.twitter.com/oauth/authorize"
#'
#' consumerKey<-"XXXXXXXXXXXXXXXXXXXXXXXXXX"
#'
#' consumerSecret<-"XXXXXXXXXXXXXXXXXXXXXXXXXX"
#'
#' oauthKey<-"XXXXXXXXXXXXXXXXXXXXXXXXXX"
#'
#' oauthSecret<-"XXXXXXXXXXXXXXXXXXXXXXXXXX"
#'
#' tw_oauth<-OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,
#'      oauthKey=oauthKey,oauthSecret=oauthSecret,requestURL=requestURL,
#'      accessURL=accessURL,authURL=authURL)
#'
#' 3. Autoryzujemy manualnie poprzez:
#' tw_oauth$handshake(cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")).
#'
#' 4. Nastepnie zapisuje sie zmienna do pliku przy pomocy funkcji save(tw_oauth,file).
#'
#' @return Zwraca invisible NULL oraz komunikat z informacja o wynikach sesji pobierania.
#'
#' @author Piotr Smuda
#'

wybory_pobierz_tw<-function(sciezka_zapis=getwd(),sciezka_dostep_api,czas){
   #wczytujemy plik z dostepem do api
   load(sciezka_dostep_api)

   if(stri_sub(sciezka_zapis,-1)!="/"){
      sciezka_zapis<-paste0(sciezka_zapis,"/")
   }

   #ścieżka dla folderu gdzie będą zapisywane dane
   sciezka<-paste0(sciezka_zapis,"Twitter/")
   dir.create(sciezka,showWarnings=FALSE)
   sciezka<-paste0(sciezka,"tweety/")
   dir.create(sciezka,showWarnings=FALSE)

   #ustalamy folder working directory
   old<-getwd()
   setwd(sciezka)
   on.exit(setwd(old))

   filterStream(file=paste0("tweety-",Sys.Date(),".json"),
      track=slownik_tw,timeout=czas*3600,oauth=tw_oauth,language="pl")

   return(invisible(NULL))
}