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

#' Vector of DNA nucleotide bases as characters.
#' @export
nuc_bases = function() c("A", "T", "C", "G")

#' Create all tuples of size `n`.
#' @param tsize Tuple size (e.g. 3 for codons)
#' @param alphabet The alphabet. Default are DNA bases.
#' @examples codons = all_tuples(3)
#' @export
all_tuples = function(tsize, alphabet = nuc_bases()) {
  # TODO see also seqinr::words
  r_all_tuples(tsize, alphabet)
}

#' Convert string into vector of characters.
#'
#' @param s String, e.g. sequence
#' @return String as vector of characters
as.char.vector = function(s) {
  strsplit(s, "")[[1]] # as char vector
}

#' Normalize a nucleotide sequence. Default is DNA bases and upper-case letters.
#'
#' Note that unknown bases (e.g. letter K) do not raise any exceptions.
#' @param seq A vector of sequences as a string to be processed.
#' @param RNA If true, RNA bases are used, i.e. T becomes U.
#' If false (default), DNA bases are used, i.e. U becomes T.
#' @param lowercase If true, all letters are converted to lowercase (ATG -> atg).
#' IF false (default), all letters will be uppercase (atg -> ATG)
#' @return Normalized string
#' @examples normalize("auggcc") # will yield ATGGCC.
#' @export
normalize = function(seqv, RNA = FALSE, lowercase = FALSE) {
  # TODO impl. in Rust
  r = sapply(seqv, function(seq) {
    seq1 = if (lowercase) tolower(seq) else toupper(seq)
    seq2 = if (lowercase) {
      if (RNA) gsub("t", "u", seq1) else gsub("u", "t", seq1)
    } else {
      if (RNA) gsub("T", "U", seq1) else gsub("U", "T", seq1)
    }
    seq2
  })
  as.vector(r)
}

#' Frequencies of tuples (codon).
#'
#' Calculates the absolute frequencies of tuples in a vector.
#'
#' @param tuples Vector of tuples (codons)
#' @return Data frame with two columns:
#' 1. tuple, 2. number of tuples in vector
#' @export
tuples_freq = function(tuples) {
  tsize = nchar(tuples[1])
  ftuples = factor(tuples, levels = all_tuples(tsize))

  df = data.frame(ftuples)
  df = as.data.frame(table(ftuples))
  colnames(df) = c("tuple", "freq")
  class(df) = append("gcat.codon.usage", class(df)) # order is important.
  df
}

# Register new generic function:
#' @export
amino_acids = function(x, ...) UseMethod("amino_acids", x)

#' @export
amino_acids.default = function(x, value, ...) {
  stop("Implementation for this type not supported.")
}

#' Translate codons into amino acids.
#' @param codons Vector of codons.
#' @param numcode The ncbi genetic code number for translation.
#' By default the standard genetic code (1) is used.
#' See https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi
#' @export
amino_acids.character = function(codons, numcode = 1) {
  aa = sapply(codons, function(codon) {
    ncodon = normalize(codon, RNA = FALSE, lowercase = FALSE)
    cc = seqinr::s2c(ncodon)
    seqinr::translate(cc, numcode = numcode)
  })
  aa
}