#'
#' Zamieszcza wektor odleglosci miedzy filmami w bazie
#' 
#' Funkcja \code{WstawOdlegloscDoBazy} umieszcza wektor wszystkich odleglosci 
#' miedzy dwoma filmami w bazie. 
#' 
#' @usage WstawOdlegloscDoBazy(IDFilm1, IDFilm2, sciezkaDoBazy)
#'
#'@param IDFilm1, \code{IDFilm2} ID filmow dla ktorych chcemy liczyc odleglosci
#'@param sciezkaDoBazy sciezka do bazy, w ktorej przechowywane sa informacje o 
#'filmach
#'
#'@details Funkcja laczy sie z baza i korzystajac z funkcji 
#'\code{OdleglosciMiedzyFilmami} oblicza wektor odleglosci oraz umieszcza go 
#'w bazie 
#'
#'@return TRUE jesli proces sie powiedzie, FALSE w przeciwnym przypadku
#'
#'@author Karolina Wyszynska
#'
#'@import RSQLite
#'
#'@examples
#'sciezkaDoBazy <- "PelnaBazaFilmow.sql" # Nie zadziala bez wczesniej wypelnionej bazy
#'WstawOdlegloscDoBazy(23456, 4000, sciezkaDoBazy) 
#'

WstawOdlegloscDoBazy <- function(IDFilm1, IDFilm2, sciezkaDoBazy){
  
  odleglosc <- OdleglosciMiedzyFilmami(IDFilm1, IDFilm2, sciezkaDoBazy)
  if(is.na(odleglosc)[1]){
    return(FALSE)      
  }
  
  polaczenie <- dbConnect(SQLite(), sciezkaDoBazy)
  dbSendQuery(polaczenie, paste0("INSERT INTO Odleglosci VALUES (", 
                                 paste(odleglosc, collapse=","), ")"))
  dbDisconnect(polaczenie)
  TRUE
}