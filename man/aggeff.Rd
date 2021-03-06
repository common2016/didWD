% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggeff.R
\name{aggeff}
\alias{aggeff}
\title{Aggrgate Treatment Effects}
\usage{
aggeff(fit, xname, se = "white2", type = "sss")
}
\arguments{
\item{fit}{A object returned by \code{plm::plm}.}

\item{xname}{a character vector including names of independent variables.}

\item{se}{methods computing standard error. \code{'white2'} indicates s.e. clustered in
individuals, \code{'white1'} indicate common White s.e., and \code{'arellano'} means
the vcov matrix is block diagonal. See the help of \code{plm::vcovHC}.}

\item{type}{\code{"HC0", "sss", "HC1", "HC2", "HC3", "HC4"}. See the help of \code{plm::vcovHC}.}
}
\value{
a numeric vector including mean, standard error, t value and p value.
}
\description{
Aggrgate Treatment Effects
}
