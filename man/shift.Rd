% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manip.R
\name{shift}
\alias{shift}
\title{Shift each tuple (or sequence) in a vector for \code{k} positions to the left
(AUG -> UGA).}
\usage{
shift(tuples, k)
}
\arguments{
\item{tuples}{Vector of strings}

\item{k}{Number of positions. \code{k} equal to 0 means no shift. A negative number shifts to the right (AUG -> GAU).}
}
\value{
Vector with shifted tuples.
}
\description{
Shift each tuple (or sequence) in a vector for \code{k} positions to the left
(AUG -> UGA).
}
