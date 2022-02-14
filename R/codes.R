# Copyright 2021 by the authors.
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
DEFAULT_ID  <- "unkn."
#' Create a code from a list of tuples.
#'
#' @param tuples Vector of tuples or codons as strings, e.g. c("AUC", "GCA").
#' @param id A brief description of the code. Default is "unkn.".
#' @param unique.set A boulian value. If true, all dublicates are removed from the set.
#' @param sorted.set A boulian value. If true, the set is sorted in alphabetical order.
#'
#' @return A 'gcat.code' object. Derived from vector with two attributs: 1) id,  2) tsize.
#' @export
code = function(tuples, id = DEFAULT_ID, unique.set= T, sorted.set= T) {
  code <- tuples
  if(unique.set) {code = unique(code)}
  if(sorted.set) {code = code[order(code)]}
   # Sort them
  tsize = unique(sapply(code, nchar)) # Letter per tuple
  tsize = sort(tsize, decreasing = T) # Letter per tuple

  attr(code, "id") = id
  attr(code, "tsize") = tsize
  class(code) = "gcat.code"
  code
}

#' Returns the id of a code or the default id ("unkn.")
#'
#' @param code A 'gcat.code' object
#'
#' @return The id of a 'gcat.code'
#'
#' @examples get.id(code(c("ACG", "CC"), id = "Test 1"))
#'
#' @export
get.id <- function(code) {
  if(is.null(attr(code, 'id'))) DEFAULT_ID else attr(code, 'id')
}

#' Pretty prints a code.
#'
#' @param code
#'
#' @return
#' @export
print.gcat.code = function(code) {
  s = paste(attr(code, 'id'), ": ", toString(unlist(code)), sep = "")
  print.default(s)
}

#' Give a summary for a code.
#'
#' @param code
#'
#' @return
#' @export
summary.gcat.code = function(code) {
  n = length(code)
  r = list(
    paste0("Code: ", attr(code, "id")),
    paste0(n, " tuples: ", paste(code, collapse = ", "))
  )
  cat(r[[1]])
  cat("\n")
  cat(r[[2]])
  cat("\n")
  a = alphabet(code)
  cat(paste0("Seq-alphabet: ", toString(unlist(a$letters)), " (", a$type, ")"))
  if (3 %in% attr(code, "tsize") && (a$is.rna || a$is.dna)) {
    codons <- Filter(function (w) nchar(w)==3, code)
    aa = amino_acids(codons)
    na = length(aa)
    r[[3]] = paste0(na, " amino acids: ", paste(aa, collapse = ", "))
    cat("\n")
    cat(r[[3]])
  }
  cat("\n")
  return(invisible(NULL))
}

#' Translate codons of a code into amino acids.
#'
#' @param code A code with tuples of size 3.
#' @param numcode The ncbi genetic code number for translation.
#' By default the standard genetic code (1) is used.
#' See https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
#' @export
amino_acids.gcat.code = function(code, numcode = 1) {
  a <- alphabet(code)
  if (3 %in% attr(code, "tsize") && (a$is.rna || a$is.dna)) {
    codons <- Filter(function (w) nchar(w)==3, code)
    return(amino_acids.character(codons, numcode))
  }

  stop("Tuple size must be 3 for amino acid translation!")
}

#' Read one or more codes from file.
#'
#' The structure of the file is: \cr
#' # header with short description \cr
#' 1, ACA, ACT, ... \cr
#' 2, ACT, ATT, ... \cr
#'
#' @param filename Textfile with codes in it.
#' @param tsize The size of the tuples in the code. Default is 3.
#' @param skip_codeid_col If true the first column is left out.
#' @return A list of codes.
#' @export
read_codes = function(filename, tsize = 3,
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
#' @param filename Textfile with codes in it.
#' @param codes List of codes.
#' @param header Header of text file. Default is "# codes".
#' @export
write_codes = function(filename, codes, header = "# codes") {
  write(header, filename)
  res = lapply(codes, function(c) {
    s = paste(get.id(c), ", ", toString(unlist(c)), sep = "")
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
random_code = function(size = 20, tsize = 3,
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

code_usage = function(seq, code) {
  h = gcatbase::classify(seq, code)
  length(h[h == 1]) / length(h)
}