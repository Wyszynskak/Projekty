#'
#'Dodaje pusta tabele odleglosci do bazy filmow
#'
#'Funkcja \code{DodajTabeleOdleglosci} do podanej bazy danych dodaje pusta
#'tabele przechowujaca odleglosci miedzy filmami
#'
#'@usage DodajTabeleOdleglosci(sciezkaDoBazy)
#'
#'@param sciezkaDoBazy sciezka do pliku z baza
#'@param nazwaTabeli nazwa dodawanej tabeli
#'
#'@details Funkcja dodaje pusta tabele z zainicjowanymi kolumnami:
#'IDFilm1, IDFilm2, dRokProdukcji, dOpisIMDB, dGatunki, dScenarzysci, dRezyserowie,
#'dKraje, dAktorzy, dAktorzyGlowni, dDAktorzyGwiazdy, dProducenci, dOcenaIMDB,
#' dOcenaMezczyzniPonizej18, dOcenaKobietyPonizej18, dOcenaMezczyzni18_29,
#' dOcenaKobiety18_29, dOcenaMezczyzni30_44, dOcenaKobiety30_44,
#' dOcenaMezczyzniPowyzej45, dOcenaKobietyPowyzej45.
#' W odpowiednich kolumnach mozna potem przetrzymywac odleglosci miedzy dwoma
#' filmami ze wzgledu na okreslona kategorie.
#'
#'@return Funkcja zwraca wartosc TRUE jesli proces sie powiedzie.
#'
#'@author Karolina Wyszynska
#'@examples
#' #Nie wyoluj jesli nie masz takiej bazy
#' DodajTabeleOdleglosci("BazaFilmow.sql")
#'
#'@import RSQLite
#'


DodajTabeleOdleglosci <- function(sciezkaDoBazy, nazwaTabeli="Odleglosci"){
  polaczenie <- dbConnect(SQLite(), sciezkaDoBazy)
  on.exit(dbDisconnect(polaczenie))
  dbSendQuery(polaczenie, 
              paste0("CREATE TABLE ", nazwaTabeli, "(
                           IDFilm1 bigint NOT NULL,
                           IDFilm2 bigint NOT NULL,
                           dRokProdukcji double,
                           dOpisIMDB double,
                           dGatunki double,
                           dScenarzysci double,
                           dRezyserowie double,
                           dKraje double,
                           dAktorzy double,
                           dAktorzyGlowni double,
                           dAktorzyGwiazdy double,
                           dProducenci double,
                           dOcenaIMDB double,
                           dOcenaMezczyzniPonizej18 double,
                           dOcenaKobietyPonizej18 double,
                           dOcenaMezczyzni18_29 double,
                           dOcenaKobiety18_29 double,
                           dOcenaMezczyzni30_44 double,
                           dOcenaKobiety30_44 double,
                           dOcenaMezczyzniPowyzej45 double,
                           dOcenaKobietyPowyzej45 double,
                           PRIMARY KEY(IDFilm1,IDFilm2))"))

  TRUE
}


