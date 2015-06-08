#' Wstawianie informacji o filmie do bazy
#'
#' Funkcja \code{WstawFilmDoBazy} sciaga informacje o filmie
#' i wstawia je do odpowiedniej bazy danych
#'
#' @usage
#' \code{WstawFilmDoBazy(linkDoFilmu, sciezkaDoBazy)}
#'
#' @param linkDoFilmu link do interesujacego nas filmu w serwisie IMBD
#' @param sciezkaDoBazy sciezka do bazy znajdujacej sie w pliku
#'
#' @details \code{WstawFilmDoBazy} sciagajac informacje o podanym filmie
#' korzysta przy tym z funkcji \code{PobierzFilm}. Wartosci pobrane z wyniku
#' lokuje w bazie analogicznej do \code{StworzBaze}. Funkcja sama nie tworzy bazy.
#' Funkcja nie przyjmuje filmow krotkometrazowych.
#'
#' @return Zwraca wartosc TRUE jesli zapis sie powiodl, FALSE
#' wraz z komentarzem w przeciwnym przypadku.
#'
#' @examples
#' sciezka <- getwd()
#' StworzBaze(paste0(sciezka, "bazaFilmow.sql"))
#' WstawFilmDoBazy("http://www.imdb.com/title/tt0395699/", paste0(sciezka, "bazaFilmow.sql"))
#'


WstawFilmDoBazy <- function(linkDoFilmu, sciezkaDoBazy){

  polaczenie <- dbConnect(SQLite( ), dbname=sciezkaDoBazy)

  #Pobieramy informacje o filmie
  if(!CzyFilm(linkDoFilmu)){return(FALSE)}
  film <- PobierzFilm(linkDoFilmu)

  #Jesli krotkometrazowy lub zle odczytany zwroc false
  if(is.null(film)){return(FALSE)}

  #Sprawdzam czy film jest w bazie
  temp <- dbGetQuery(polaczenie,
                     paste0('SELECT TytulAngielski FROM Filmy WHERE
                            TytulAngielski="', film[[1]]$tytul_ang, '"'))
  if(nrow(temp)!=0){print("Taki film jest juz w bazie!"); return(FALSE)}

  #Wstawiamy informacje do tabeli Filmy
  if(nrow(dbGetQuery(polaczenie, "SELECT * FROM Filmy limit 2"))==0){
    IDFilm <- 1
  } else{
    IDFilm <- dbGetQuery(polaczenie, "SELECT MAX(IDFilm) FROM Filmy") + 1
  }

  IDFilm <- as.integer(IDFilm)
  temp <- data.frame(IDFilm=IDFilm)
  temp2 <- cbind(temp, film[[1]])
  dbWriteTable(polaczenie, "pomocnicza", temp2)
  dbSendQuery(polaczenie, "INSERT INTO Filmy (IDFilm, TytulPolski,
                          TytulAngielski, RokProdukcji, OpisIMDB, Budzet)
                          SELECT IDFilm, tytul_pol, tytul_ang, rok_produkcji,
                          opis_IMDB, budzet FROM pomocnicza")
  dbRemoveTable(polaczenie, "pomocnicza")

  #Wstawiamy informacje do tabeli Ocena
  if(!is.null(film[[2]])){
    temp2 <- stri_paste(as.character(film[[2]]), collapse=",")
    temp2 <- stri_paste(IDFilm, temp2, sep=",")
    dbSendQuery(polaczenie, paste0("INSERT INTO Oceny values (",temp2,")"))
  }

  #Wstawiamy gatunki do tabeli Gatunki
  gatunki <- film[[3]]
  for(i in seq_along(gatunki)){
        temp <- dbGetQuery(polaczenie, paste0('SELECT * FROM Gatunki WHERE
                                              IDFilm=', IDFilm, ' AND
                                              Nazwa="', gatunki[i], '"'))
        if(nrow(temp) != 0){
          print(paste0("Gatunek ", gatunki[i], " dla filmu ", IDFilm, " juz
                       jest w bazie"))
          next
        } else {
          dbSendQuery(polaczenie, paste0("INSERT INTO Gatunki VALUES (",
                                        IDFilm, ',"' , gatunki[i], '")'))
        }
  }

  dbDisconnect(polaczenie)


  #Wstawiamy rezyserow do tabeli Rezyserowie
  if(!is.null(film[[4]])){
    rezyserowie <- unique(film[[4]])
    polaczenie <- dbConnect(SQLite(), dbname=sciezkaDoBazy)
    for(i in 1:nrow(rezyserowie)){

      temp <- dbGetQuery(polaczenie, paste0('SELECT * FROM Rezyserowie WHERE
                                            IDFilm=', IDFilm, ' AND Imie="',
                                            rezyserowie[i,1], '" AND
                                            Nazwisko="', rezyserowie[i,2],'"'))
      if(nrow(temp) != 0){
        print(paste0("Rezyser ", rezyserowie[i,c(1,2)], " dla filmu ", IDFilm,
                     " juz jest w bazie"))
        next
      } else{
        dbSendQuery(polaczenie, paste0("INSERT INTO Rezyserowie VALUES (",
                                       IDFilm, ',"' , rezyserowie[i,1], '","',
                                       rezyserowie[i,2], '")'))
      }
    }
    dbDisconnect(polaczenie)
  }


  #Wstawiamy scenarzystow do tabeli Scenarzysci
  if(!is.null(film[[5]])){
    scenarzysci <- unique(film[[5]])
    polaczenie <- dbConnect(SQLite(), dbname=sciezkaDoBazy)
    for(i in 1:nrow(scenarzysci)){

      temp <- dbGetQuery(polaczenie, paste0('SELECT * FROM Scenarzysci WHERE
                                            IDFilm=', IDFilm, ' AND
                                            Imie="', scenarzysci[i,1], '" AND
                                            Nazwisko="', scenarzysci[i,2], '"'))
      if(nrow(temp) != 0){
        print(paste0("Scenarzysta ", scenarzysci[i,c(1,2)], " dla filmu ",
                     IDFilm, " juz jest w bazie"))
        next
      } else{
        dbSendQuery(polaczenie, paste0("INSERT INTO Scenarzysci VALUES (",
                                      IDFilm, ',"' , scenarzysci[i,1], '","',
                                      scenarzysci[i,2], '")'))
      }
    }
    dbDisconnect(polaczenie)
  }

  #Wstawiamy producentow do tabeli Producenci
  producenci <- unique(film[[6]])
  polaczenie <- dbConnect(SQLite(), dbname=sciezkaDoBazy)
  for(i in seq_along(producenci)){
    temp <- dbGetQuery(polaczenie, paste0('SELECT * FROM Producenci WHERE
                                          IDFilm=', IDFilm, ' AND Nazwa="',
                                          producenci[i],'"'))
    if(nrow(temp) != 0){
      print(paste0("Producent ", producenci[i], "dla filmu ", IDFilm, "jest
                    juz w bazie."))
      next;
    } else{
      dbSendQuery(polaczenie, paste0("INSERT INTO Producenci values (",
                                     IDFilm, ',"', producenci[i],'")'))
    }
  }
  dbDisconnect(polaczenie)


  #Wstawiamy kraje do tabeli Kraje
  kraje <- unique(film[[7]])
  polaczenie <- dbConnect(SQLite(), dbname=sciezkaDoBazy)
  for(i in seq_along(kraje)){
    temp <- dbGetQuery(polaczenie, paste0('SELECT * FROM Kraje WHERE IDFilm=',
                                        IDFilm, ' AND Nazwa="', kraje[i], '"'))
    if(nrow(temp) != 0){
      print(paste0("Kraj ", kraje[i], " dla filmu ", IDFilm, " jest juz w bazie."
                   ))
      next
    } else{
      dbSendQuery(polaczenie, paste0("INSERT INTO Kraje values (", IDFilm, ',"',
                                     kraje[i], '")'))
    }
  }
  dbDisconnect(polaczenie)


  #Wstawiamy aktorow do tabeli Aktorzy
  aktorzy <- film[[8]]
  tmp <- duplicated(aktorzy[ ,c(1,2)])
  if(any(tmp)){ aktorzy <- aktorzy[-which(tmp), ]}

  for(i in 1:nrow(aktorzy)){
    polaczenie <- dbConnect(SQLite(), dbname=sciezkaDoBazy)
    temp <- dbGetQuery(polaczenie, paste0('SELECT * FROM Aktorzy WHERE IDFilm=',
                                          IDFilm, ' AND Imie="', aktorzy[i,1],
                                          '" AND Nazwisko="', aktorzy[i,2], '"'))
    if(nrow(temp)!=0){
      print(paste0("Aktor ", aktorzy[i, c(1,2)], " dla filmu ", IDFilm, "jest
                   juz w bazie."))
      next
    } else{
      dbSendQuery(polaczenie, paste0("INSERT INTO Aktorzy VALUES (", IDFilm,
                                     ',"', aktorzy[i, 1], '","', aktorzy[i, 2],
                                     '"', aktorzy[i, 3], ",", aktorzy[i, 4], ")"
                                     ))
    }
    dbDisconnect(polaczenie)
  }

  print(paste0("Film o ID: ", IDFilm, " zostal poprawnie wstawiony do bazy."))

  TRUE
}
