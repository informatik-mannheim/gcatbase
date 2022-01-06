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

#' Shift each tuple (or sequence) in a vector for `k` positions to the right.
#'
#' @param k Number of positions. `k` equal to 0 means no shift. A negative number shifts to the left.
#' @param tuples Vector of strings
#'
#' @return Vector with shifted tuples.
#' @export
shift = function(k, tuples) {
  sapply(tuples, function(tuple) {
    n = nchar(tuple)
    suffix = substr(tuple, k + 1, n)
    prefix = substr(tuple, 1, k)
    paste(suffix, prefix, sep = "")
  })
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
  n = as.integer(nchar(seq) / tsize) * tsize # number of tuples
  if (n < tsize) { # if there is no tuple at all...
    return(c()) # an empty vector is returned.
  }
  starts = seq(1, n, by = tsize)
  # chop it up:
  sapply(starts, function(ii) substr(seq, ii, ii + (tsize - 1)))
}