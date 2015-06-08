#'
#' Wczytywanie odleglosci z CSV do bazy
#'
#' Funkcja \code{ZCSVDoBazy} zczytuje wektory odleglosci pomiedzy filmami z pliku
#' CSV do bazy danych
#'
#' @usage ZCSVDoBazy(sciezkaDoBazy, sciezkaDoCSV, wczytujCo, nazwaTabeli)
#'
#'@param sciezkaDoBazy sciezka do bazy, w ktorej trzymamy informacje o filmach
#'@param sciezkaDoCSV sciezka do pliku '.csv', w ktyrm tymczasowo przechowujemy dane
#'@param wczytujCo ile wierszy z pliku chcemy wczytywac na raz
#'@param nazwaTabeli do jakiej tabeli w bazie chcemy to wczytac (musi miec kolumny)
#'analogiczne jak tabela ze \code{DodajTabeleOdleglosci}
#'
#'@return
#'Zwraca wartosc TRUE
#'
#'@author Karolina Wyszynska
#'
#'@examples
#'sciezkaDoBazy <- "BazaFilmow.sql"
#'sciezkaDoCSV <- "PrzechowajOdleglosci.csv"
#'ZCSVDoBazy(sciezkaDoBazy, sciezkaDoCSV, wczytujCo=100, "Odleglosci")
#'
#'@import RSQLite



ZCSVDoBazy <- function(sciezkaDoBazy, sciezkaDoCSV, wczytujCo, nazwaTabeli){
  #Otwieramy plik z którego bedziemy czytać
  f <- file(sciezkaDoCSV, "r")
  on.exit(close(f))
  nazwyKolumn <- c("IDFilm1", "IDFilm2", "dRokProdukcji", ",dOpisIMDB",
                   "dGatunki", "dScenarzysci", "dRezyserowie", "dKraje",
                   "dAktorzy", "dAktorzyGlowni", "dAktorzyGwiazdy",
                   "dProducenci", "dOcenaIMDB", "dOcenaMezczyzniPonizej18",
                   "dOcenaKobietyPonizej18", "dOcenaMezczyzni18_29",
                   "dOcenaKobiety18_29", "dOcenaMezczyzni30_44",
                   "dOcenaKobiety30_44", "dOcenaMezczyzniPowyzej45",
                   "dOcenaKobietyPowyzej45")

  suma <- 0

  #Wczytujemy co wczytujCo wierszy
  while(TRUE){
      try(dane <- read.table(f, head=FALSE, nrow=wczytujCo), silent=TRUE)
      if(is.null(dane)){break;}

      colnames(dane) <- nazwyKolumn

      #Wstawiamy je do tabeli pomocniczej
      polaczenie <- dbConnect(SQLite(), sciezkaDoBazy)
      dbWriteTable(polaczenie, "pomocnicza", dane)
      on.exit({
        pol <- dbConnect(SQLite(), sciezkaDoBazy)
        try(dbRemoveTable(pol, "pomocnicza"), silent=TRUE)
        dbDisconnect(pol)
      }, add=TRUE)

      #Następnie kopiujemy do wybranej tabeli
      dbSendQuery(polaczenie, paste0("INSERT INTO ",  nazwaTabeli, " SELECT * FROM
                                      pomocnicza"))
      dbRemoveTable(polaczenie, "pomocnicza")
      dbDisconnect(polaczenie)

      suma <- suma + wczytujCo
      print(suma)
      dane <- NULL
  }

  TRUE
}
