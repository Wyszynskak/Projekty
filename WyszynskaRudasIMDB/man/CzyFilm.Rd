% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/CzyFilm.R
\name{CzyFilm}
\alias{CzyFilm}
\title{Sprawdzenie czy link prowadzi do filmu}
\usage{
\code{CzyFilm(linkDoFilmu)}
}
\arguments{
\item{linkDoFilmu}{link do interesujacego nas filmu w serwisie IMBD (
struktura jak w przykladzie)}
}
\value{
Zwraca wartosc TRUE jesli pod linkiem znajduje sie film, FALSE
wraz z komentarzem w przeciwnym przypadku.
}
\description{
Funkcja \code{CzyFilm} sprawdza czy pod podanym linkiem znajduje sie
informacja o filmie
}
\examples{
CzyFilm("http://www.imdb.com/title/tt0395699/")
}
\author{
Krzysztof Rudas
}

