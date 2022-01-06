#' Frequencies of tuples (codon).
#'
#' Calculates the absolute frequencies of tuples in a vector.
#'
#' @param tuples Vector of tuples (codons)
#' @return Data frame with two columns:
#' 1. tuple, 2. number of tuples in vector
#' @export
tuples.freq = function(tuples) {
  tsize = nchar(tuples[1])
  ftuples = factor(tuples, levels = all_tuples(tsize)) 

  df = data.frame(ftuples)
  df = as.data.frame(table(ftuples))
  colnames(df) = c("tuple", "freq")
  class(df) = append("gcat.codon.usage", class(df)) # order is important.
  df
}