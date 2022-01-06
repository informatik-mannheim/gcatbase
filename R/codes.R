#' Create a code from a list of tuples.
#'
#' @param tuples Vector of tuples or codons as strings, e.g. c("AUC", "GCA").
#' @param id A brief description of the code. Default is "unkn.".
#'
#' @return List with two elements: 1) id, 2) codons.
#' @export
code = function(tuples, id = "unkn.") {
  utuples = unique(tuples)
  utuples = utuples[order(utuples)] # Sort them
  tsize = nchar(utuples[1]) # Letter per tuple
  code = list(id = id, tuples = utuples, tsize = tsize)
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

#' Read one or more codes from file.
#'
#' The structure of the file is: \cr
#' # header with short description \cr
#' 1, ACA, ACT, ... \cr
#' 2, ACT, ATT, ... \cr
#'
#' @param filename
#' @param tsize The size of the tuples in the code. Default is 3.
#' @param skip_codeid_col If true the first column is left out.
#'
#' @return
#' @export
read.codes = function(filename, tsize = 3,
                      skip_codeid_col = TRUE) {
  # TODO: stringAsFactors does not work (bug?).
  # read.csv reads factors, which are later manually converted
  # to characters.
  A = read.csv(filename,
    skip = 1, header = FALSE, # First line is a comment.
    stringsAsFactors = FALSE,
    comment.char = "#",
    sep = ",",
    strip.white = TRUE
  ) # Codons as text.
  m = nrow(A) # Number of rows.
  n = ncol(A) # Number of columns.
  s = if (skip_codeid_col) 1 else 2 # Index if first column
  e = n # Index of last column
  codes = lapply(1:m, function(i) {
    cid = if (skip_codeid_col) i else A[i, 1]
    code(id = cid, tuples = as.character(A[i, s:e]))
  })
  codes
}


#' Write one or more codes to a file.
#'
#' Details see `read.codes`.
#'
#' @param filename
#' @param codes List of codes.
#' @param header Header of text file. Default is "# codes".
#'
#' @export
write.codes = function(filename, codes, header = "# codes") {
  write(header, filename)
  res = lapply(codes, function(c) {
    s = paste(c$id, ", ", toString(unlist(c$tuples)), sep = "")
    write(s, filename, append = TRUE, ncolumns = 1000)
  })
}

#' Random code.
#'
#' @param size Number of codons in the code. Default is 20.
#' @param tsize Number of bases per tuple. Default is 3.
#' @param id Brief description of the code. Default is "unkn. rnd.".
#'
#' @return Vector or string based tuples.
#' @export
random.code = function(size = 20, tsize = 3,
                       id = "unkn. rnd") {
  bases = c("A", "T", "C", "G")
  l = rep(list(bases), tsize)
  tuples_chars = expand.grid(l) # Create all combinations (as chars).
  # Make tuple as strings:
  all_tuples = apply(tuples_chars, 1, function(k) paste(k, collapse = ""))
  tuples = sample(x = all_tuples, size = size, replace = FALSE)
  code(id = id, tuples = tuples)
}

#' Map a sequence to classes "in code" (1) or "out of code" (0).
#'
#' @param seq Sequence as string.
#' @param code Code (instance of `gcat.code`)
#'
#' @return Vector of 0s and 1s.
#' @export
classify = function(seq, code) {
  tvec = split(seq, code$tsize)
  sapply(tvec, function(t) {
    if (is.element(t, code$tuples)) 1 else 0
  })
}

code.usage = function(seq, code) {
  h = gcatbase::classify(seq, code)
  length(h[h == 1]) / length(h)
}