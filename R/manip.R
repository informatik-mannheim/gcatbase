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

#' Shift each tuple (or sequence) in a vector for `k` positions to the left
#' (AUG -> UGA).
#'
#' @param tuples Vector of strings
#' @param k Number of positions. `k` equal to 0 means no shift. A negative number shifts to the right (AUG -> GAU).
#'
#' @return Vector with shifted tuples.
#' @export
shift = function(tuples, k) {
  s <- sapply(tuples, function(tuple) {
    n = nchar(tuple)
    if (k < 0) {
      k = n + k # Shift for n-|k| to the left
    }

    k <- ((k - 1) %% n) + 1
    prefix = substr(tuple, 1, k)
    suffix = substr(tuple, k + 1, n)
    # Change order:
    as.character(paste(suffix, prefix, sep = ""))
  })

  code(as.vector(s), id = paste0(get.id(tuples), "_shifted_by_", toString(k)), unique.set = F, sorted.set = F) # Return as code object.
}

#' Complementary DNA tuples.
#'
#' @param tuples Vector of strings where the words consists of DNA bases
#' (A, T, C, G) in upper case. Invoke `normalize` on strings prior to call `compl` if needed.
#' @return Vector of same lengths like tuples where each tuple contains the
#' complementary bases for each letter in the string. I.e. A<->T and C<->G.
#' @export
compl = function(tuples) {
  s = sapply(tuples, function(t) {
    cv = as.char.vector(t)
    ccv = sapply(cv, function(b) {
      # complementary base
      cb = switch(b,
                  "A" = "T",
                  "T" = "A",
                  "C" = "G",
                  "G" = "C"
      )
      if (nchar(cb) == 1) cb else stop(paste0("Invalid base: ", b, ". Only A, T, G, C allowed."))
    })
    paste0(ccv, collapse = "") # vector to string again
  })

  as.vector(s) # Strip off keys.
}

#' Reversed and complementary tuples.
#'
#' Example: (ATTGA, GCG) -> (TCAAT, CGC)
#'
#' @param tuples Vector of tuples. Note: tuples are not required
#' to have the same tuple size.
#' @return Vector of same lengths like tuples where bases are in reverse order
#' and complementary.
#' @export
rev_compl = function(tuples) {
  s = sapply(compl(tuples), function(t) {
    cv = as.char.vector(t)
    rcv = rev(cv) # reverse order
    paste0(rcv, collapse = "") # vector to string again
  })
  as.vector(s)  # Strip off keys.
}

#' Split string sequence into tupels (codons).
#'
#' A sequence can be split either by passing the parameter `tsize` or the
#' parameter `sep`. If `sep` is set, `tsize` is ignored.
#'
#' In case of `tsize` only tuples of exact this size are returned.
#' If the last tuple has less than `tsize` bases it is skipped.
#'
#' On the other hand, if `set` is used, all tuples are returned. There
#' is also no check if the tuples have the same size.
#'
#' @param seq Sequence as a string.
#' @param tsize Tuple size. Default: 3 for codons.
#' @param sep Separator, e.g. ","
#' @param regexp.sep Separator as regular expression, e.g. "\[, \]+"
#'
#' @return Vector of tuples. Each tuple is a string.
#' @export
split = function(seq, tsize = 3, sep = NULL, regexp.sep = NULL) {
  if (is.null(regexp.sep) && is.null(sep)) { # split by tuple size
    tsize = round(tsize, 0) # Ensure integers
    if (tsize < 1) {
      stop(paste0("Tuple size must not be less than 1! ", tsize, " < 1"))
    }
    n = as.integer(nchar(seq) / tsize) * tsize # number of tuples
    if (n < tsize) { # if there is no tuple at all...
      return(c()) # an empty vector is returned.
    }
    starts = seq(1, n, by = tsize)
    # chop it up:
    sapply(starts, function(ii) substr(seq, ii, ii + (tsize - 1)))
  } else if (is.null(sep)) { # split by separator
    stringr::str_split(seq, pattern = regexp.sep)[[1]]
  } else { # split by separator
    strsplit(seq, split = sep)[[1]]
  }
}

#' Truncate a string at the beginning.
#'
#' This is useful for a frame shift.
#'
#' @param seq Sequence as a string to truncate.
#' @param k Characters to truncate: 0, 1, 2, etc.
#' @return String or sequence where k nucleotides are truncated at the beginning.
#' @export
truncate = function(seq, k) {
  substr(seq, 1 + k, nchar(seq))
}