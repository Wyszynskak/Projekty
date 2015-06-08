#' Tworzenie bazy przechowujacej informacje o filmach
#' 
#' Funkcja \code{StworzBaze} tworzy relacyjna baze danych przechowujaca 
#' informacje o filamch
#' 
#' \code{StworzBaze(sciezka_do_bazy)}
#' 
#' @param sciezka_do_bazy sciezka do nowotworzonej bazy
#' @details \code{StworzBaze} generuje tabele przechowujace informacje min. 
#' o tytulach, gatunkach, aktorach. Oprocz tabeli przechwujacych dane, 
#' konstruowane sa rowniez tabele wiele do wielu, ktore umozliwiaja powiazanie 
#' miedzy tabelami przechowujacymi rozne informacje.
#' 
#' @return Zwraca wartosc TRUE swiadczaca o utworzeniu relacyjnej bazy danych o 
#' zadanych tabelach.
#' 
#' @author Krzysztof Rudas
#' @author Karolina Wyszynska
#' 
#' @examples
#' sciezka <- getwd()
#' StworzBaze(paste0(sciezka, "bazaFilmow.sql"))
#'
#'

StworzBaze<-function(sciezka_do_bazy){
  
  baza_filmow<-dbConnect(SQLite(),dbname=sciezka_do_bazy)
  
  #Tworzymy tabele
  
  #Filmy
  dbSendQuery(baza_filmow, "CREATE TABLE Filmy (
              IDFilm bigint NOT NULL PRIMARY KEY, 
              TytulPolski nvarchar(100) NOT NULL,
              TytulAngielski nvarchar(100) NOT NULL, 
              RokProdukcji int , 
              OpisIMDB text, 
              Budzet money)")
  
  #Gatunki
  dbSendQuery(baza_filmow,"CREATE TABLE Gatunki (
              IDFilm bigint NOT NULL, 
              Nazwa nvarchar(100) NOT NULL, PRIMARY KEY(IDFilm,Nazwa))")
  
  #Scenarzysci
  dbSendQuery(baza_filmow,"CREATE TABLE Scenarzysci(
              IDFilm bigint NOT NULL, 
              Imie nvarchar(100) NOT NULL,
              Nazwisko nvarchar(100) NOT NULL, PRIMARY KEY(IDFilm, Imie, Nazwisko))")
  
  #Rezyserowie
  dbSendQuery(baza_filmow,"CREATE TABLE Rezyserowie(
              IDFilm bigint NOT NULL, 
              Imie nvarchar(100) NOT NULL,
              Nazwisko nvarchar(100) NOT NULL, PRIMARY KEY(IDFilm, Imie, Nazwisko))")
  
  #Oceny
  dbSendQuery(baza_filmow,"CREATE TABLE Oceny(
              IDFilm bigint NOT NULL PRIMARY KEY,
              OcenaIMDB double, 
              MezczyzniPonizej18 double,
              KobietyPonizej18 double,
              Mezczyzni18_29 double,
              Kobiety18_29 double,
              Mezczyzni30_44 double,
              Kobiety30_44 double,
              MezczyzniPowyzej45 double,
              KobietyPowyzej45 double)")
  
  #Kraje
  dbSendQuery(baza_filmow,"CREATE TABLE Kraje (
              IDFilm bigint NOT NULL, 
              Nazwa nvarchar(100) NOT NULL, PRIMARY KEY(IDFilm,Nazwa))")
  
  #Aktorzy
  dbSendQuery(baza_filmow,"CREATE TABLE Aktorzy(
              IDFilm bigint NOT NULL, 
              Imie nvarchar(100) NOT NULL,
              Nazwisko nvarchar(100) NOT NULL,
              czyGlowny bit NOT NULL, 
              czyGwiazda bit NOT NULL, PRIMARY KEY(IDFilm, Imie, Nazwisko, czyGlowny, czyGwiazda))")
  
  #Producenci
  dbSendQuery(baza_filmow,"CREATE TABLE Producenci (
              IDFilm bigint NOT NULL, 
              Nazwa nvarchar(100) NOT NULL, PRIMARY KEY(IDFilm,Nazwa))")
  
  dbDisconnect(baza_filmow)
}
