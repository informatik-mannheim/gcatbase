% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list.R
\name{flatten}
\alias{flatten}
\title{Process elements in a list and flatten the results.}
\usage{
flatten(l, op)
}
\arguments{
\item{l}{A list (e.g. a fasta file)}

\item{op}{A function to apply on the list's entries.
The function's return type must be a vector (or skalar).}
}
\value{
Vector of elements.
}
\description{
Process elements in a list and flatten the results.
}
