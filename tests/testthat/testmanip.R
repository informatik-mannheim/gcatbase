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

# Test file
library(testthat)

context("sequence manipulation")

test_that("frame 0", {
  s = "CAGTTTGAGTAT"
  expect_equal(truncate(s, 0), "CAGTTTGAGTAT")
})

test_that("frame 1", {
  s = "CAGTTTGAGTAT"
  expect_equal(truncate(s, 1), "AGTTTGAGTAT")
})

test_that("frame 2", {
  s = "CAGTTTGAGTAT"
  expect_equal(truncate(s, 2), "GTTTGAGTAT")
})

test_that("frame 3", {
  s = "CAGTTTGAGTAT"
  expect_equal(truncate(s, 3), "TTTGAGTAT")
})

test_that("shift 0", {
  s = "CAGTTTGAGTAT"
  # Ignore attributes
  expect_equivalent(shift(s, 0), "CAGTTTGAGTAT")
})

test_that("shift 1", {
  s = "CAGTTTGAGTAT"
  expect_equivalent(shift(s, 1), "AGTTTGAGTATC")
})

test_that("shift 2", {
  s = "CAGTTTGAGTAT"
  expect_equivalent(shift(s, 2), "GTTTGAGTATCA")
})

test_that("shift -1", {
  s = "CAGTTTGAGTAT"
  expect_equivalent(shift(s, -1), "TCAGTTTGAGTA")
})

test_that("vector shift 1", {
  s = c("CAGTTTGAGTAT", "AUG")
  expect_equivalent(shift(s, 2), c("GTTTGAGTATCA", "GAU"))
})

test_that("split 0", {
  s = "GACT"
  expect_error(split(s, 0))
})

test_that("split 1", {
  s = "GACT"
  expect_equal(split(s, 1), c("G", "A", "C", "T"))
})

test_that("split 2", {
  s = "GACT"
  expect_equal(split(s, 2), c("GA", "CT"))
})

test_that("split 3", {
  s = "GACT"
  expect_equal(split(s, 3), c("GAC"))
})

test_that("split 1.6", {
  s = "GCATA"
  expect_equal(split(s, 1.6), c("GC", "AT"))
})


context("self complementary")

test_that("complementary 1", {
  tv = "A"
  expect_equal(compl(tv), "T")
})

test_that("complementary 2", {
  tv = c("A", "G")
  expect_equal(compl(tv), c("T", "C"))
})

test_that("self complementary single", {
  tv = "A"
  expect_equal(rev_compl(tv), "T")
})

test_that("self complementary codon", {
  tv = c("TAG")
  expect_equal(rev_compl(tv), c("CTA"))
})

test_that("self complementary 2 codons different size", {
  tv = c("CAG", "ATTG")
  expect_equal(rev_compl(tv), c("CTG", "CAAT"))
})

test_that("self complementary 2 codons again", {
  tv = c("CAG", "ATT")
  expect_equal(rev_compl(tv), c("CTG", "AAT"))
})