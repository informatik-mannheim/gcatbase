% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tuples.R
\name{amino_acids.character}
\alias{amino_acids.character}
\title{Translate codons into amino acids.}
\usage{
\method{amino_acids}{character}(x, numcode = 1, ...)
}
\arguments{
\item{x}{Vector of codons.}

\item{numcode}{The ncbi genetic code number for translation.
By default the standard genetic code (1) is used.
See https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi}

\item{...}{
  Arguments passed on to \code{\link[=amino_acids]{amino_acids}}
  \describe{
    \item{\code{}}{}
  }}
}
\description{
Translate codons into amino acids.
}
