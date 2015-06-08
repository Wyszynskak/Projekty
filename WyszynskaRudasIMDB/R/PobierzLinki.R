#' Wyszukiwanie linkow do filmow z bazy IMDb
#'
#' Funkcja \code{PobierzLinki} wysyla zapytanie do wyszukiwarki IMDB 
#' w zaleznosci od wybranych gatunkow
#'
#' @usage
#' \code{PobierzLinki(gatunki = c("action", "adventure", "animation", "biography", "comedy", "crime", 
#'                                "documentary", "drama", "family", "fantasy", "film_noir", "history", 
#'                                "horror", "music", "musical", "mystery", "romance", "sci_fi", 
#'                                "sport", "thriller", "war", "western"))}
#'
#' @param gatunki wektor typu character gatunkow zgodnych z dostepnymi w zaawansowanej wyszukiwarce serwisu IMDB
#' @details \code{PobierzLinki} dla kazdego podanego gatunku wysyla zapytanie do wyszukiwarki 
#' IMDB oraz zbiera stamtad liste linkow do wyszukanych filmow. 
#' Wynik zapisuje w pliku oddzielnym dla kazdego gatunku.
#'
#' @return Zwraca wartosc TRUE or FALSE w zaleznosci od tego czy wykonanie sie powiedzie czy nie
#' @author Karolina Wyszynska
#'
#' @examples
#' PobierzLinki("adventure")
#'



PobierzLinki <- function( gatunki = c("action", "adventure", "animation", "biography", "comedy", "crime", 
                                                                "documentary", "drama", "family", "fantasy", "film_noir", "history", 
                                                                "horror", "music", "musical", "mystery", "romance", "sci_fi", 
                                                                "sport", "thriller", "war", "western")){

  linkiDoWyszukiwarki <- character(length(gatunki))
  
  for(i in seq_along(gatunki)){
    linkiDoWyszukiwarki[i] <- paste0("http://www.imdb.com/search/title?genres=", 
                                   gatunki[i], 
                                   "&title_type=feature,tv_movie,documentary,video")
    
    strona <- html(linkiDoWyszukiwarki[i])
    wezly <- html_nodes(strona, "#left")
    ilosc <- stri_match_all_regex(html_text(wezly), "[0-9]+.[0-9]+")[[1]][2] %>%
             stri_replace_all_regex(",", "") %>% as.integer()
    
    nazwa <- paste0(gatunki[i],".txt")
    file.create(nazwa)
    
    if(ilosc < 100000){
      
      k <- seq(1, ilosc, 100)
      for(j in seq_along(k)){
        strona <- html(paste0("http://www.imdb.com/search/title?genres=", gatunki[i], 
               "&start=", k[j], 
               "&title_type=feature,tv_movie,tv_series,documentary,video"))
      wezly <- html_nodes(strona, "a")
      linki <- html_attr(wezly, "href")
      ktore <- which(!is.na(stri_match_all_regex(linki,"/title/tt[0-9]{7}/$")))
      linki <- linki[ktore]
      linki <- unique(linki)
      f <- file(nazwa, "a")
      writeLines(stri_paste(linki, collapse="\n"), f)
      close(f)
      }

    }else{
      k <- seq(1, 99999, 100)
      for(j in seq_along(k)){
        strona <- html(paste0("http://www.imdb.com/search/title?genres=", gatunki[i], 
                              "&sort=alpha&start=", k[j], 
                              "&title_type=feature,tv_movie,tv_series,documentary,video"))
        wezly <- html_nodes(strona, "a")
        linki <- html_attr(wezly, "href")
        ktore <- which(!is.na(stri_match_all_regex(linki,"/title/tt[0-9]{7}/$")))
        linki <- linki[ktore]
        linki <- unique(linki)
        f <- file(nazwa, "a")
        writeLines(stri_paste(linki, collapse="\n"), f)
        close(f)
      }
      
      k <- seq(1, ilosc-100000, 100)
      for(j in seq_along(k)){
        strona <- html(paste0("http://www.imdb.com/search/title?genres=", gatunki[i], 
                              "&sort=alpha,desc&start=", k[j], 
                              "&title_type=feature,tv_movie,tv_series,documentary,video"))
        wezly <- html_nodes(strona, "a")
        linki <- html_attr(wezly, "href")
        ktore <- which(!is.na(stri_match_all_regex(linki,"/title/tt[0-9]{7}/$")))
        linki <- linki[ktore]
        linki <- unique(linki)
        f <- file(nazwa, "a")
        writeLines(stri_paste(linki, collapse="\n"), f)
        close(f)
      }
    }
    cat(gatunki[i], " zostal zapisany")
  }
  
  print("Proces zakonczony powodzeniem")
  TRUE
}



