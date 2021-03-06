% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/dRezyserowie.R
\name{dRezyserowie}
\alias{dRezyserowie}
\title{Liczenie odleglosci dla rezyserow}
\usage{
dRezyserowie(IDFilm1, IDFilm2, sciezkaDoBazy)
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
Funkcja \code{dRezyserowie} liczy odleglosc miedzy rezyserami dwoch filmow z bazy
}
\details{
Funkcja wyciaga ID dwoch filmow z bazy. Jesli ktorys z nich nie ma rezyserow odleglosc
miedzy nimi wynosi 0.8, jesli tak nie jest miara jest liczona zgodnie z funkcja PorownajWektory.
}
\examples{
#Nie wywoluj jesli nie masz takiej bazy
dRezyserowie(4000,2461,"BazaFilmow.sql")
dRezyserowie(23456,2461,"BazaFilmow.sql")
}
\author{
Krzysztof Rudas
}

