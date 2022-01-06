#' Workaround for GCATR
#'
#' @param k How many shifts (0, 1, 2)
#' @param codons
#'
#' @return
#' @export
shift = function(k, tuples) {
  sapply(tuples, function(tuple) {
    n = nchar(tuple)
    suffix = substr(tuple, k + 1, n)
    prefix = substr(tuple, 1, k)
    paste(suffix, prefix, sep = "")
  })
}