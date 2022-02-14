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

#' 
#' @export 
alphabet = function(s) {
  # see also Biostrings::alphabet()
  vs = as.character(unlist(sapply(s, function(si) strsplit(si, "")[[1]])))

  uv = unique(vs) # no duplicates
  uv = uv[order(uv)] # sorted
  is.dna <- all(uv %in% c("A", "C", "G", "T"))
  is.rna <- all(uv %in% c("A", "C", "G", "U"))

  atype <- if (is.dna && is.rna) "DNA and/or RNA" else if (is.dna) "DNA" else if (is.rna) "RNA" else "unknown"

  alphabet = list(letters = uv, type = atype, is.dna = is.dna, is.rna = is.rna)
  class(alphabet) = "gcat.alphabet"
  alphabet
}

#' Pretty prints an alphabet.
#'
#' @param alphabet
#'
#' @return
#' @export
print.gcat.alphabet = function(alphabet) {
  s = paste(toString(unlist(alphabet$letters)), " (", alphabet$type, ")", sep = "")
  print.default(s)
}
