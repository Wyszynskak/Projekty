#'
#' Wektor odleglosci miedzy filmami w bazie
#'
#' Funkcja \code{OdleglosciMiedzyFilmami} oblicza wektor wszystkich odleglosci
#' miedzy dwoma filmami
#'
#' @usage OdleglosciMiedzyFilmami(IDFilm1, IDFilm2, sciezkaDoBazy)
#'
#'@param IDFilm1, \code{IDFilm2} ID filmow dla ktorych chcemy liczyc odleglosci
#'@param sciezkaDoBazy sciezka do bazy, w ktorej przechowywane sa informacje o
#'filmach
#'
#'@details Funkcja laczy sie z baza i korzystajac ze wszystkich funkcji typu d*
#'dolaczonych do pakietu oblicza wektor odleglosci miedzy dwoma filmami
#'
#'@return Wektor typu double o wartosciach odpowiadajacych wartosciom kolumn
#'z tabeli utworzonej przez \code{DodajTabeleOdleglosci}
#'
#'@author Karolina Wyszynska
#'
#'@import RSQLite
#'
#'@examples
#'sciezkaDoBazy <- "PelnaBazaFilmow.sql" # Nie zadziala bez wczesniej wypelnionej bazy
#'OdleglosciMiedzyFilmami(23456, 4000, sciezkaDoBazy)
#'

OdleglosciMiedzyFilmami <- function(IDFilm1, IDFilm2, sciezkaDoBazy){
  polaczenie <- dbConnect(SQLite(), sciezkaDoBazy)
  on.exit(dbDisconnect(polaczenie))
  if(!SprawdzCzywBazie(IDFilm1, IDFilm2, polaczenie)){
    print("Jednego z tych filmow nie ma w bazie.")
    return(NA)
  }

  options(scipen=999)
  c(IDFilm1, IDFilm2, dRokProdukcji(IDFilm1, IDFilm2, sciezkaDoBazy),
                dOpisIMDB(IDFilm1, IDFilm2, sciezkaDoBazy),
                dGatunki(IDFilm1, IDFilm2, sciezkaDoBazy),
                dScenarzysci(IDFilm1, IDFilm2, sciezkaDoBazy),
                dRezyserowie(IDFilm1, IDFilm2, sciezkaDoBazy),
                dKraje(IDFilm1, IDFilm2, sciezkaDoBazy),
                dAktorzy(IDFilm1, IDFilm2, sciezkaDoBazy),
                dAktorzyGlowni(IDFilm1, IDFilm2, sciezkaDoBazy),
                dAktorzyGwiazdy(IDFilm1, IDFilm2, sciezkaDoBazy),
                dProducenci(IDFilm1, IDFilm2, sciezkaDoBazy),
                dOcenaIMDB(IDFilm1, IDFilm2, sciezkaDoBazy),
                dOcenaMezczyzniPonizej18(IDFilm1, IDFilm2, sciezkaDoBazy),
                dOcenaKobietyPonizej18(IDFilm1, IDFilm2, sciezkaDoBazy),
                dOcenaMezczyzni18_29(IDFilm1, IDFilm2, sciezkaDoBazy),
                dOcenaKobiety18_29(IDFilm1, IDFilm2, sciezkaDoBazy),
                dOcenaMezczyzni30_44(IDFilm1, IDFilm2, sciezkaDoBazy),
                dOcenaKobiety30_44(IDFilm1, IDFilm2, sciezkaDoBazy),
                dOcenaMezczyzniPowyzej45(IDFilm1, IDFilm2, sciezkaDoBazy),
                dOcenaKobietyPowyzej45(IDFilm1, IDFilm2, sciezkaDoBazy))
}

