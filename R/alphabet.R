# Copyright 2022 by the authors.
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

#' Get the alphabet defined by the letters in the string.
#' @param s String containing all letters of the alphabet.
#' @return List of letters.
#' @examples 
#' alphabet("UCAG") # Defines RNA alphabet.
#' alphabet("ab") # Defines an alphabet of letters a and b.
#' @export
alphabet = function(s) {
  # see also Biostrings::alphabet()
  vs = as.character(unlist(sapply(s, function(si) strsplit(si, "")[[1]])))

  uv = unique(vs) # no duplicates
  uv = uv[order(uv)] # sorted
  is_dna = all(uv %in% c("A", "C", "G", "T"))
  is_rna = all(uv %in% c("A", "C", "G", "U"))

  atype = if (is_dna && is_rna) {
    "DNA and/or RNA"
  } else if (is_dna) {
    "DNA"
  } else if (is_rna) "RNA" else "unknown"

  alphabet = list(letters = uv, type = atype, is.dna = is_dna, is.rna = is_rna)
  class(alphabet) = "gcat.alphabet"
  alphabet
}

# Note: Parameter must be the same in all generic functions, here `x`.

#' Pretty prints an alphabet.
#'
#' @param x The alphabet
#' @inheritDotParams base::print
#' @return
#' @export
print.gcat.alphabet = function(x, ...) {
  s = paste(toString(unlist(x$letters)), " (", x$type, ")", sep = "")
  print.default(s)
}