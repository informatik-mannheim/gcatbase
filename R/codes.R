#' Create a code from a list of tuples.
#'
#' @param tuples Vector of tuples or codons as strings, e.g. c("AUC", "GCA").
#' @param id A brief description of the code. Default is "unkn.".
#'
#' @return List with two elements: 1) id, 2) codons.
#' @export
code = function(tuples, id = "unkn.") {
  code = list(id = id, tuples = tuples)
  class(code) = "gcat.code"
  code
}

#' Pretty prints a code.
#'
#' @param code 
#'
#' @return
#' @export
print.gcat.code = function(code) {
  s = paste(code$id, ": ", toString(unlist(code$tuples)), sep = "")
  print.default(s)
}

#' Pretty prints a code.
#'
#' @param code 
#'
#' @return
#' @export
summary.gcat.code = function(code) {
  s = "not implemented"
  summary.default(s)
}
