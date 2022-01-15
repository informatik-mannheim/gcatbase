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
  sapply(tuples, function(tuple) {
    n = nchar(tuple)
    if (k < 0) {
      k = n + k # Shift for n-|k| to the left
    }
    prefix = substr(tuple, 1, k)
    suffix = substr(tuple, k + 1, n)
    # Change order:
    as.character(paste(suffix, prefix, sep = ""))
  })
}

#' Complementary base.
#'
#' @param base b in upper case and DNA bases
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
  as.vector(s) # Remove field names.
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
  s = sapply(tuples, function(t) {
    cv = as.char.vector(t)
    rcv = rev(cv) # reverse order
    rcv = sapply(rcv, function(b) compl(b)) # complementary base
    paste0(rcv, collapse = "") # vector to string again
  })
  as.vector(s) # Remove field names.
}

#' Split string sequence into tupels (codons).
#'
#' If the last tuple has less than `tsize` bases it is skipped.
#'
#' @param seq Sequence as a string.
#' @param tsize Tuple size. Default: 3 for codons.
#'
#' @return Vector of tuples. Each tuple is a string.
#' @export
split = function(seq, tsize = 3) {
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
}

#' Truncate a string at the beginning.
#'
#' This is useful for a frame shift.
#'
#' @param k Characters to truncate: 0, 1, 2, etc.
#' @return String or sequence where k nucleotides are truncated at the beginning.
#' @export
truncate = function(seq, k) {
  substr(seq, 1 + k, nchar(seq))
}