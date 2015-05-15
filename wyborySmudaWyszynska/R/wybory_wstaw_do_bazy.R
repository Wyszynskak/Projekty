#' Uzupelnianie bazy ze wskaznikami
#'
#' Funkcja \code{wybory_wstaw_do_bazy} uzupelnia baze relacyjna wyliczonymi wskaznikami dla okresu bez analizy.
#'
#' @usage \code{wybory_wstaw_do_bazy(okres,sciezka_do_bazy,sciezka_odczyt=getwd())}
#'
#' @param okres okres, za ktory jest liczony wskaznik; mozliwosci: "dzien", "tydzien", "miesiac"
#' @param sciezka_do_bazy sciezka do pliku, w ktorym jest baza, ktora zostanie uzupelniona
#' @param sciezka_odczyt sciezka do folderu bazowego z wszystkimi plikami
#'
#' @details
#' Dzieki funkcjom pomocnicznym sprawdzany jest okres, w ktorym byla
#' przeprowadzona ostatnia analiza, a nastepnie dla brakujacych okresow zostaja
#' obliczone i wstawione wskazniki do bazy ze wskaznikami.
#'
#' @return Zwraca invisible NULL.
#'
#' @author Piotr Smuda, Karolina Wyszynska
#'

wybory_wstaw_do_bazy<-function(okres,sciezka_do_bazy,sciezka_odczyt=getwd()){
   #generujemy wzorce dla okresów bez analizy
   wzorce<-generuj_date(okres,analiza_pobierania(okres,sciezka_do_bazy))
   #łączymy się z bazą
   baza_wybory<-dbConnect(SQLite(),dbname=sciezka_do_bazy)
   #trzeba usunąć rekordy z pierwszym wzorcem, bo w tym wzorcu analiza może być
   #niepełna
   do_usuniecia<-wzorce[1]
   dbSendQuery(baza_wybory,paste0("DELETE FROM ", stri_trans_totitle(okres),
      " WHERE Wzorzec=\"",do_usuniecia,"\""))
   #rozłączamy się z bazą
   suppressWarnings(dbDisconnect(baza_wybory))
   #wstawiamy do bazy wszystkie nowe analizy okresów
   lapply(wzorce,wybory_wskazniki_do_bazy,okres,sciezka_do_bazy,sciezka_odczyt)
   return(invisible(NULL))
}

analiza_pobierania<-function(okres,sciezka_do_bazy){
   #łączymy się z bazą
   baza_wybory<-dbConnect(SQLite(),dbname=sciezka_do_bazy)

   wskazniki<-dbReadTable(baza_wybory,stri_trans_totitle(okres))

   #rozłączamy się z bazą
   suppressWarnings(dbDisconnect(baza_wybory))

   #określamy kiedy zaczęliśmy zbiórkę danych
   poczatek.zbiorki.danych<-"2015-03-14"
   if(okres=="tydzien"){
      poczatek.zbiorki.danych<-as.POSIXlt(poczatek.zbiorki.danych)
      poczatek.zbiorki.danych<-ceiling((poczatek.zbiorki.danych$yday+4)/7)
      poczatek.zbiorki.danych<-paste0("2015-",poczatek.zbiorki.danych)
   }
   else if(okres=="miesiac"){
      poczatek.zbiorki.danych<-strftime(as.POSIXlt(poczatek.zbiorki.danych),"%Y-%m")
   }
   if(nrow(wskazniki)==0){ #gdy tabela pusta, to pobieramy od 1 dnia zbiórki danych
      return(poczatek.zbiorki.danych)
   }
   analiza<-wskazniki %>% group_by(IDKandydata) %>%
      summarise(ostatnia.analiza=max(Wzorzec))
   analiza<-analiza$ostatnia.analiza
   kandydaci<-slownik_fb_kandydaci
   if(length(analiza)!=length(kandydaci)){ #gdy brakuje analizy dla jakiegoś kandydata
      #to analizujemy od 1 dnia zbiórki danych
      return(poczatek.zbiorki.danych)
   }
   analiza<-min(analiza)
   return(analiza)
}

generuj_dzisiejszy_wzor<-function(okres){
   if(okres=="dzien"){
      wzor<-as.character(Sys.Date())
   }
   else if(okres=="tydzien"){
      wzor<-Sys.Date()
      wzor<-as.POSIXlt(wzor)
      wzor<-ceiling((wzor$yday+4)/7)
      wzor<-paste0("2015-",wzor)
   }
   else if(okres=="miesiac"){
      wzor<-Sys.Date()
      wzor<-strftime(wzor, "%Y-%m")
   }
   return(wzor)
}

generuj_date<-function(okres,od,do=generuj_dzisiejszy_wzor(okres)){
   if(okres=="dzien"){
      od<-as.Date(od)
      do<-as.Date(do)
      wektor<-as.character(seq(od,do,1))
   }
   else if(okres=="tydzien"){
      przedrostek<-unlist(stri_extract_all_regex(od,"^[^-]+"))
      przedrostek<-paste0(przedrostek,"-")
      od<-unlist(stri_extract_all_regex(od,"[^-]+$"))
      do<-unlist(stri_extract_all_regex(do,"[^-]+$"))
      wektor<-od:do
      wektor<-paste0(przedrostek,wektor)
   }
   else if(okres=="miesiac"){
      przedrostek<-unlist(stri_extract_all_regex(od,"^[^-]+"))
      przedrostek<-paste0(przedrostek,"-")
      od<-unlist(stri_extract_all_regex(od,"[^-]+$"))
      do<-unlist(stri_extract_all_regex(do,"[^-]+$"))
      wektor<-as.character(od:do)
      wektor<-sapply(wektor,function(element){
            if(stri_length(element)==1){
               element<-paste0("0",element)
            }
            else {
               element<-element
            }
         },USE.NAMES=FALSE)
      wektor<-paste0(przedrostek,wektor)
   }
   return(wektor)
}