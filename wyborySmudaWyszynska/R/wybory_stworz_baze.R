#' Tworzenie bazy ze wskaznikami
#'
#' Funkcja \code{wybory_stworz_baze} tworzy szkielet bazy relacyjnej ze wskaznikami dla kandydatow.
#'
#' @usage \code{wybory_stworz_baze(sciezka_do_bazy)}
#'
#' @param sciezka_do_bazy sciezka do pliku, w ktorym ma zostac zapisana baza
#'
#' @details
#' Utworzona baza ze wskaznikami zawiera tabelki:
#'
#' 1. Kandydaci o kolumnach IDKandydata (klucz glowny, klucz obcy), ImieINazwisko.
#'
#' 2. Dzien o kolumnach Wzorzec (klucz glowny), IDKandydata (klucz glowny, klucz obcy),
#' IloscTweetow, WydzwiekTweetow, IloscPolubienFB, SzybkoscPolubienFB, IloscPostowFB,
#' SredniaLikePostFB, SredniaKomentarzPostFB, SredniaSharePostFB, IloscGoogle, SredniGoogle,
#' TytulyInteria, TytulyOnet, TytulyWP, TytulyPortale, ArtykulyInteria, ArtykulyOnet,
#' ArtykulyWP, ArtykulyPortale.
#'
#' 3. Tydzien o kolumnach Wzorzec (klucz glowny), IDKandydata (klucz glowny, klucz obcy),
#' IloscTweetow, WydzwiekTweetow, IloscPolubienFB, SzybkoscPolubienFB, IloscPostowFB,
#' SredniaLikePostFB, SredniaKomentarzPostFB, SredniaSharePostFB, IloscGoogle, SredniGoogle,
#' TytulyInteria, TytulyOnet, TytulyWP, TytulyPortale, ArtykulyInteria, ArtykulyOnet,
#' ArtykulyWP, ArtykulyPortale.
#'
#' 4. Miesiac o kolumnach Wzorzec (klucz glowny), IDKandydata (klucz glowny, klucz obcy),
#' IloscTweetow, WydzwiekTweetow, IloscPolubienFB, SzybkoscPolubienFB, IloscPostowFB,
#' SredniaLikePostFB, SredniaKomentarzPostFB, SredniaSharePostFB, IloscGoogle, SredniGoogle,
#' TytulyInteria, TytulyOnet, TytulyWP, TytulyPortale, ArtykulyInteria, ArtykulyOnet,
#' ArtykulyWP, ArtykulyPortale.
#'
#' @return Zwraca invisible NULL.
#'
#' @author Piotr Smuda
#'

wybory_stworz_baze<-function(sciezka_do_bazy){

   #łączymy się z bazą
   baza_wybory<-dbConnect(SQLite(),dbname=sciezka_do_bazy)

   #tworzymy tabele

   #Kandydaci
   dbSendQuery(baza_wybory,"CREATE TABLE Kandydaci (IDKandydata int NOT NULL PRIMARY KEY,
      ImieINazwisko nvarchar(100) NOT NULL)")

   #Dzień
   dbSendQuery(baza_wybory,"CREATE TABLE Dzien (Wzorzec nvarchar(15) NOT NULL,
      IDKandydata int NOT NULL, IloscTweetow real, WydzwiekTweetow real,
      IloscPolubienFB real, SzybkoscPolubienFB real, IloscPostowFB real, SredniaLikePostFB real,
      SredniaKomentarzPostFB real, SredniaSharePostFB real, IloscGoogle real, SredniGoogle real,
      TytulyInteria real, TytulyOnet real, TytulyWP real, TytulyPortale real,
      ArtykulyInteria real, ArtykulyOnet real, ArtykulyWP real, ArtykulyPortale real,
      PRIMARY KEY (Wzorzec, IDKandydata), FOREIGN KEY (IDKandydata) REFERENCES Kandydaci(IDKandydata))")

   #Tydzień
   dbSendQuery(baza_wybory,"CREATE TABLE Tydzien (Wzorzec nvarchar(15) NOT NULL,
      IDKandydata int NOT NULL, IloscTweetow real, WydzwiekTweetow real,
      IloscPolubienFB real, SzybkoscPolubienFB real, IloscPostowFB real, SredniaLikePostFB real,
      SredniaKomentarzPostFB real, SredniaSharePostFB real, IloscGoogle real, SredniGoogle real,
      TytulyInteria real, TytulyOnet real, TytulyWP real, TytulyPortale real,
      ArtykulyInteria real, ArtykulyOnet real, ArtykulyWP real, ArtykulyPortale real,
      PRIMARY KEY (Wzorzec, IDKandydata), FOREIGN KEY (IDKandydata) REFERENCES Kandydaci(IDKandydata))")

   #Miesiąc
   dbSendQuery(baza_wybory,"CREATE TABLE Miesiac (Wzorzec nvarchar(15) NOT NULL,
      IDKandydata int NOT NULL, IloscTweetow real, WydzwiekTweetow real,
      IloscPolubienFB real, SzybkoscPolubienFB real, IloscPostowFB real, SredniaLikePostFB real,
      SredniaKomentarzPostFB real, SredniaSharePostFB real, IloscGoogle real, SredniGoogle real,
      TytulyInteria real, TytulyOnet real, TytulyWP real, TytulyPortale real,
      ArtykulyInteria real, ArtykulyOnet real, ArtykulyWP real, ArtykulyPortale real,
      PRIMARY KEY (Wzorzec, IDKandydata), FOREIGN KEY (IDKandydata) REFERENCES Kandydaci(IDKandydata))")

   #uzupełniamy tabele Kandydaci korzystając ze słownika
   kandydaci<-slownik_fb_kandydaci
   pomocnicza<-data.frame(IDKandydata=1:length(kandydaci),ImieINazwisko=kandydaci)
   dbWriteTable(baza_wybory,"pomocnicza",pomocnicza)
   dbSendQuery(baza_wybory,"INSERT INTO Kandydaci (IDKandydata, ImieINazwisko)
      SELECT IDKandydata, ImieINazwisko FROM pomocnicza")
   dbRemoveTable(baza_wybory,"pomocnicza")

   #rozłączamy się z bazą
   suppressWarnings(dbDisconnect(baza_wybory))

   return(invisible(NULL))
}