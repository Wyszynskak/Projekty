% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/dProducenci.R
\name{dProducenci}
\alias{dProducenci}
\title{Liczenie odleglosci dla prodocentow}
\usage{
dProducenci(IDFilm1, IDFilm2, sciezkaDoBazy)
}
\arguments{
\item{IDFilm1}{- id pierwszego filmu}

\item{IDFilm2}{- id drugiego filmu}

\item{sciezkaDoBazy}{- sciezka do bazy filmow}
}
\value{
Funkcja zwraca wartosc liczbowa opisujaca odleglosc miedzy filmami.
}
\description{
Funkcja \code{dProducenci} liczy odleglosc miedzy producentami dwoch filmow z bazy
}
\details{
Funkcja wyciaga ID dwoch filmow z bazy. Jesli ktorys z nich nie ma producentow odleglosc miedzy
nimi wynosi 0.8, jesli tak nie jest miara jest liczona zgodnie z funkcja PorownajWektory.
}
\examples{
#Nie wywoluj jesli nie masz takiej bazy
dProducenci(4000,2461,"BazaFilmow.sql")
dProducenci(23456,2461,"BazaFilmow.sql")
}
\author{
Krzysztof Rudas
}

