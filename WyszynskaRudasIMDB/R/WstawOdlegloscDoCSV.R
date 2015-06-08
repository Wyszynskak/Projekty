#'
#' Zamieszcza wektor odleglosci miedzy filmami w pliku CSV
#' 
#' Funkcja \code{WstawOdlegloscDoCSV} umieszcza wektor wszystkich odleglosci 
#' miedzy dwoma filmami w wybranym pliku typu '.csv'. 
#' 
#' @usage WstawOdlegloscDoCSV(IDFilm1, IDFilm2, sciezkaDoBazy, sciezkaDoCSV)
#'
#'@param IDFilm1, \code{IDFilm2} ID filmow dla ktorych chcemy liczyc odleglosci
#'@param sciezkaDoBazy sciezka do bazy, w ktorej przechowywane sa informacje o 
#'filmach
#'@param sciezkaDoCSV sciezka do pliku typu '.csv', w ktorym chcemy trzymac dane
#'
#'@details Funkcja laczy sie z baza i korzystajac z funkcji 
#'\code{OdleglosciMiedzyFilmami} oblicza wektor odleglosci oraz umieszcza go 
#'w pliku CSV 
#'
#'@return TRUE jesli proces sie powiedzie, FALSE w przeciwnym przypadku
#'
#'@author Karolina Wyszynska
#'
#'@import RSQLite
#'
#'@examples
#'sciezkaDoCSV <- "Odleglosci.csv"
#'if(!file.exists(sciezkaDoCSV)){file.create(sciezkaDoCSV)}
#'sciezkaDoBazy <- "PelnaBazaFilmow.sql" # Nie zadziala bez wczesniej wypelnionej bazy
#'WstawOdlegloscDoCSV(23456, 4000, sciezkaDoBazy) 
#'


WstawOdlegloscDoCSV <- function(IDFilm1, IDFilm2, sciezkaDoBazy, sciezkaDoCSV){
  odleglosc <- OdleglosciMiedzyFilmami(IDFilm1, IDFilm2, sciezkaDoBazy)
  if(is.na(odleglosc)[1]){
    return(FALSE)      
  }
  
  odleglosc <- matrix(odleglosc, nrow=1)
  
  write.table(odleglosc, file=sciezkaDoCSV, append=TRUE, col.names=FALSE, 
              row.names=FALSE)
  TRUE
}