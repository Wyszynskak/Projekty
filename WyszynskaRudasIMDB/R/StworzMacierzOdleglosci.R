#'
#' Liczenie macierzy  odleglosci dla zadanego filmu
#'
#' Funkcja \code{StworzMacierzOdleglosci} zwraca macierz odleglosci 10 filmow
#' najblizszych wybranemu filmowi.
#'
#' @usage StworzMacierzOdleglosci(IDFilm, wektorWspolczynnikow, sciezkaDoBazy)
#'
#' @param IDFilm ID filmu na podstawie, ktorego chcemy wybrac 10 najblizszych
#' mu filmow
#' @param wektorWspolczynnikow wektor zwracany przez funkcje \code{WektorWspolczynnikow}
#' okreslajacy wage poszczegolnych rzeczy dla uzytkownika
#' @param sciezkaDoBazy sciezka do bazy, w ktorej przechowujemy informacje o filmach
#'
#' @details Funkcja operujac na bazie danych znajduje 10 najblizszych filmow dla
#' IDFilm oraz oblicza macierz odleglosci pomiedzy tymi 11 (lacznie z IDFilm) filmami
#'
#' @return Zwraca listę, której pierwszym elementem jest wyszukiwany film, a 
#' drugim macierz odleglosci z tytulami filmow w wierszach i kolumnach
#'
#' @author Karolina Wyszynska
#'
#'@examples
#' sciezkaDoBazy <- "BazaFilmow.sql"
#' IDFilm <- 126029
#' wektorWspolczynnikow <- WektorWspolczynnikow(c("Oceny", "Producenci"), "K", 22)
#' StworzMacierzOdleglosci(IDFilm, wektorWspolczynnikow, sciezkaDoBazy)
#'
#'
#'


StworzMacierzOdleglosci <- function(IDFilm, wektorWspolczynnikow, sciezkaDoBazy){

  #Polaczenie z baza danych
  polaczenie <- dbConnect(SQLite(), sciezkaDoBazy)
  on.exit({dbRemoveTable(polaczenie, "Tmp"); dbDisconnect(polaczenie)})

  #Tworzymy tabelę pomocniczą
  dbSendQuery(polaczenie, "CREATE TABLE Tmp (IDFilm1 bigint, IDFilm2 bigint,
                                               Odleglosc double,
                                               PRIMARY KEY(IDFilm1, IDFilm2))")
  #Wyciagamy wszystkie filmy porownane z IDFilm i liczymy ich odleglosc
  LiczOdleglosc(polaczenie, IDFilm, "Odleglosci", "Tmp", wektorWspolczynnikow, "OR")

  #Wybieramy ID 10 najbliższych wyjściowemu filmowi i czyścimy Tmp
  odleglosci1 <-  dbGetQuery(polaczenie, "SELECT IDFilm1, IDFilm2, Odleglosc FROM Tmp ORDER
                             BY Odleglosc LIMIT 10")
  dbSendQuery(polaczenie, "DELETE FROM Tmp WHERE IDFilm1 IN
                          (SELECT IDFilm1 FROM Tmp)")

  #Oczyszczamy ID najblizszych filmow. Musimy jeszcze znać odleglości między nimi
  ID <- unique(c(odleglosci1$IDFilm1, odleglosci1$IDFilm2))
  ID <- ID[-which(ID==IDFilm)]

  LiczOdleglosc(polaczenie, ID, "Odleglosci", "Tmp", wektorWspolczynnikow, "AND")

  odleglosci2 <- dbGetQuery(polaczenie, "SELECT IDFilm1, IDFilm2, Odleglosc
                                         FROM Tmp")
  #Z podanych odległości tworzę macierz trójkątną
  macierz <- ZlozDoMacierzy(odleglosci1, odleglosci2)

  ID <- colnames(macierz)
  Tytuly <- dbGetQuery(polaczenie, paste0(
    "SELECT IDFilm, TytulPolski FROM Filmy WHERE IDFilm IN (",
    paste(ID, collapse=", "), ")"))
  colnames(macierz) <- Tytuly$TytulPolski
  rownames(macierz) <- Tytuly$TytulPolski
  list(Film=which(ID==IDFilm), Macierz=macierz)
}



LiczOdleglosc <- function(polaczenie, Filmy, zTabeli, doTabeli, w, konektor){
  dbSendQuery(polaczenie, paste0("INSERT INTO ", doTabeli, " SELECT IDFilm1, IDFilm2, ",
                                 w[1], "*dRokProdukcji + ",
                                 w[2], "*dOpisIMDB + ",
                                 w[3], "*dGatunki + " ,
                                 w[4], "*dScenarzysci + ",
                                 w[5], "*dRezyserowie + ",
                                 w[6], "*dKraje + ",
                                 w[7], "*dAktorzy + ",
                                 w[8], "*dAktorzyGlowni + ",
                                 w[9], "*dAktorzyGwiazdy + ",
                                 w[10], "*dProducenci + ",
                                 w[11], "*dOcenaIMDB + ",
                                 w[12], "*dOcenaMezczyzniPonizej18 + ",
                                 w[13], "*dOcenaKobietyPonizej18 + ",
                                 w[14], "*dOcenaMezczyzni18_29 + ",
                                 w[15], "*dOcenaKobiety18_29 + ",
                                 w[16], "*dOcenaMezczyzni30_44 + ",
                                 w[17], "*dOcenaKobiety30_44 + ",
                                 w[18], "*dOcenaMezczyzniPowyzej45 + ",
                                 w[19], "*dOcenaKobietyPowyzej45 AS Odleglosc
                                 FROM ", zTabeli, " WHERE IDFilm1 IN (",
                                 paste(Filmy, collapse=", "), ") ", konektor, "
                                 IDFilm2 IN (",
                                 paste(Filmy, collapse=", "), ")"))
}

ZlozDoMacierzy <- function(ramka1, ramka2){
  df1 <- rbind(ramka1, ramka2)
  df2 <- df1[ ,c(2,1,3)]
  names(df2) <- names(df1)
  df <- rbind(df1, df2)
  macierz <- spread(df, IDFilm2, Odleglosc)
  rownames(macierz) <- macierz[ , 1]
  macierz <- macierz[ ,-1]
  diag(macierz) <- 0
  macierz
}


