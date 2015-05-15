#' Uzupelnianie bazy ze wskaznikami
#'
#' Funkcja \code{wybory_wskazniki_do_bazy} uzupelnia baze relacyjna wyliczonymi wskaznikami dla wskazanego okresu.
#'
#' @usage
#' \code{wybory_wskazniki_do_bazy(wzor,okres,sciezka_do_bazy,sciezka_odczyt=getwd())}
#'
#' @param wzor wzor do odczytu plikow odpowiadajacy danemu okresowi; mozliwosci: "2015-XX-XX", "2015-(X|XX)", "2015-XX"
#' @param okres okres, za ktory jest liczony wskaznik; mozliwosci: "dzien", "tydzien", "miesiac"
#' @param sciezka_do_bazy sciezka do pliku z baza, ktora ma zostac uzupelniona
#' @param sciezka_odczyt sciezka do folderu bazowego z wszystkimi plikami zawierajacymi dane
#'
#' @details
#' Dla podanego okresu funkcja ta oblicza wskazniki i wstawia je do bazy.
#'
#' @return Zwraca invisible NULL.
#'
#' @author Piotr Smuda, Karolina Wyszynska
#'

wybory_wskazniki_do_bazy <- function(wzor,okres,sciezka_do_bazy,sciezka_odczyt=getwd()){

   #IloscTweetow
   IloscTweetow <- wybory_wsk_tw_liczba_wystapien(okres,wzor,sciezka_odczyt)
   #WydzwiekTweetow
   WydzwiekTweetow <- wybory_wsk_tw_analiza_wydzwieku(okres,wzor,sciezka_odczyt)
   #IloscPolubienFB
   IloscPolubienFB <- wybory_wsk_fb_likes(okres,wzor,sciezka_odczyt)
   #SzybkoscPolubienFB
   SzybkoscPolubien <- wybory_wsk_fb_wzrost_likes(okres,wzor,sciezka_odczyt)
   #IloscPostowFB
   IloscPostowFB <- wybory_wsk_fb_liczba_postow(okres,wzor,sciezka_odczyt)
   #SredniaLikePostFB
   SredniaLikePostFB <- wybory_wsk_fb_liczba_like_post(okres,wzor,sciezka_odczyt)
   #SredniaKomentarzPostFB
   SredniaKomentarzFB <- wybory_wsk_fb_liczba_komentarz_post(okres,wzor,sciezka_odczyt)
   #SredniaSharePostFB
   SredniaSharePostFB <- wybory_wsk_fb_liczba_udostepnienie_post(okres,wzor,sciezka_odczyt)
   #IloscGoogle
   IloscGoogle <- wybory_wsk_GoogleNews(okres,wzor,sciezka_do_folderu=sciezka_odczyt)
   #SredniGoogle
   SredniGoogle <- wybory_wsk_GoogleNews(okres,wzor,typ="sredni",sciezka_odczyt)
   #TytulyInteria
   TytulyInteria <- wybory_wsk_tytuly(okres,wzor,"Interia",sciezka_odczyt)
   #TytulyOnet
   TytulyOnet <- wybory_wsk_tytuly(okres,wzor,"Onet",sciezka_odczyt)
   #TytulyWP
   TytulyWP <- wybory_wsk_tytuly(okres,wzor,"WirtualnaPolska",sciezka_odczyt)
   #TytulyPortale
   TytulyPortale <- TytulyInteria + TytulyOnet + TytulyWP
   #ArtykulyInteria
   ArtykulyInteria <- wybory_wsk_artykuly(okres,wzor,"Interia",sciezka_odczyt)
   #ArtykulyOnet
   ArtykulyOnet <- wybory_wsk_artykuly(okres,wzor,"Onet",sciezka_odczyt)
   #ArtykulyWP
   ArtykulyWP <- wybory_wsk_artykuly(okres,wzor,"WirtualnaPolska",sciezka_odczyt)
   #ArtykulyPortale
   ArtykulyPortale <- ArtykulyInteria + ArtykulyOnet + ArtykulyWP

   #łączymy się z bazą i zapamiętujemy tabele z id kandydatów
   baza_wybory<-dbConnect(SQLite(),dbname=sciezka_do_bazy)
   id_kandydaci<-dbReadTable(baza_wybory,"Kandydaci")
   kandydaci<-slownik_fb_kandydaci

   lapply(kandydaci, function(i){
      id<-id_kandydaci[id_kandydaci$ImieINazwisko==i,][,1]
      wsk <- c(IloscTweetow[i], WydzwiekTweetow[i], IloscPolubienFB[i],
         SzybkoscPolubien[i], IloscPostowFB[i], SredniaLikePostFB[i],
         SredniaKomentarzFB[i], SredniaSharePostFB[i],
         IloscGoogle[i], SredniGoogle[i], TytulyInteria[i], TytulyOnet[i],
         TytulyWP[i], TytulyPortale[i], ArtykulyInteria[i], ArtykulyOnet[i],
         ArtykulyWP[i], ArtykulyPortale[i])
      wsk <- round(wsk,2)

      #zapisujemy rekord ze wskaźnikami do bazy
      dbSendQuery(baza_wybory,paste0("INSERT INTO ", stri_trans_totitle(okres) ,
         " VALUES (\"",wzor,"\", ",id,", ",paste(wsk,collapse=", "),")"))

   })

   #rozłączamy się z bazą
   suppressWarnings(dbDisconnect(baza_wybory))

   return(invisible(NULL))
}