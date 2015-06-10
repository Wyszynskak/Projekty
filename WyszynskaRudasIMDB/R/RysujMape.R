#'
#' Rysowanie mapy odleglosci miedzy filmami
#'
#' Funkcja \code{RysujMape} rysuje mape odleglosci pomiedzy filmami
#'
#' @param macierzOdleglosci - macierz odleglosci
#' @param ukryj - ukrywa/pokazuje legende
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

RysujMape<-function(macierzOdleglosci,pokaz=TRUE)
{
   d <- as.dist(macierzOdleglosci$Macierz) 
   fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
   fit # view results
   wektor<-1:length(row.names(macierzOdleglosci$Macierz))
   # plot solution 
   x <- fit$points[,1]
   y <- fit$points[,2]
   kolory<-rep("black",11)
   size<-rep(0.85,11)
   kolory[macierzOdleglosci$Film]<-"red"
   size[macierzOdleglosci$Film]<-1.2
   par("mar"=c(0.1,0.1,0.1,0.1))
   plot(x, y, xlab="", ylab="", 
        main="",  type="n")
   text(x, y, labels = wektor, cex=size,col=kolory,font=4)
   if(pokaz==TRUE)
   {
      k<-stri_paste(1:11,row.names(macierzOdleglosci$Macierz),sep=" ",collapse="_")
      k<-unlist(stri_split_regex(k,"_"))
      kolory<-rep("black",11)
      kolory[macierzOdleglosci$Film]<-"red"
      return(legend("topleft", k,
                    bty="n", cex=.7,text.col = kolory))
   }
  
   
}
