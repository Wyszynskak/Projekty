#'
#' Rysowanie heatmapy odleglosci miedzy filmami
#'
#' Funkcja \code{RysujHeatMape} rysuje heatmape odleglosci pomiedzy filmami
#'
#' @param macierzOdleglosci - macierz odleglosci
#'
#'@details Funkcja przyjmuje jak argument macierz odleglosci generowana przez 
#'funkcje \code{StworzMacierzOdleglosci}. Na jej podstawie rysuje heatmape odleglosci
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' IDFilm <- 67484
#' wektorWspolczynnikow <- WektorWspolczynnikow(c("Oceny", "Producenci"), "K", 22)
#' sciezkaDoBazy<-paste0(getwd(),Baza_Filmow.sql)
#' RysujMape(StworzMacierzOdleglosci(IDFilm, wektorWspolczynnikow, sciezkaDoBazy))
#'

RysujHeatMape<-function(macierzOdleglosci)
{
   macierz<-as.matrix(macierzOdleglosci$Macierz)
   kolory<-rep("white",11)
   kolory[macierzOdleglosci$Film]<-"black"
   heatmap(macierz,Colv = NA,Rowv=NA,symm = TRUE,ColSideColors=kolory, RowSideColors=kolory,col=c("white",heat.colors(20)))
}