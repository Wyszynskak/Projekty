#'
#' Rysowanie mapy odleglosci miedzy filmami
#'
#' Funkcja \code{RysujMape} rysuje mape odleglosci pomiedzy filmami
#'
#' @param macierzOdleglosci - macierz odleglosci 
#'
#'@details Funkcja przyjmuje jak argument macierz odleglosci generowana przez 
#'funkcje \code{StworzMacierzOdleglosci}. Na jej podstawie rysuje mape odleglosci
#'
#'@author Krzysztof Rudas
#'
#'@examples
#' IDFilm <- 67484
#' wektorWspolczynnikow <- WektorWspolczynnikow(c("Oceny", "Producenci"), "K", 22)
#' sciezkaDoBazy<-paste0(getwd(),Baza_Filmow.sql)
#' RysujMape(StworzMacierzOdleglosci(IDFilm, wektorWspolczynnikow, sciezkaDoBazy))
#'
#'@import stringi
#'

RysujMape<-function(macierzOdleglosci)
{
   d <- as.dist(macierzOdleglosci) 
   fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
   fit # view results
   wektor<-1:length(row.names(macierzOdleglosci))
   # plot solution 
   x <- fit$points[,1]
   y <- fit$points[,2]
   par("mar"=c(0.1,0.1,0.1,0.1))
   plot(x, y, xlab="", ylab="", 
        main="",  type="n",  axes=FALSE)
   text(x, y, labels = wektor, cex=.5)
   k<-stri_paste(1:11,row.names(macierzOdleglosci),sep=" ",collapse="_")
   k<-unlist(stri_split_regex(k,"_"))
   legend("topleft", k,
          bty="n", cex=.4) 
}
