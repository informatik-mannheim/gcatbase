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

#' Convert string into vector of characters.
#' 
#' @param s String, e.g. sequence
#' @return String as vector of characters
as.char.vector = function(s) {
  strsplit(s, "")[[1]] # as char vector
}

normalize = function(seq, RNA = FALSE, lowercase = FALSE) {
  seq
}

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
# Register new generic function:
amino.acids = function(x, ...) UseMethod("amino.acids", x)

amino.acids.default = function(x, value, ...) {
stop("Implementation for this type not supported.")
}

#' Translate codons into amino acids.
#' @param codons Vector of codons.
#' @param numcode The ncbi genetic code number for translation. 
#' By default the standard genetic code (1) is used. 
#' See https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
#' @export 
amino.acids.character = function(codons, numcode = 1) {
  aa = sapply(codons, function(codon) {
    ncodon = normalize(codon, RNA = FALSE, lowercase = FALSE)
    cc = seqinr::s2c(ncodon)
    seqinr::translate(cc, numcode = numcode)
  })
  aa
}