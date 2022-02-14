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

context("codes") # Infos

test_that("Code", {
  X = code(tuples = c("AUG", "GCG"), id = "A code")
  expect_equal(attr(X, "id"), "A code")
})

context("alphabet") # Infos

test_that("Alphabet RNA", {
  s = "AUGGCCAAU"
  a = gcatbase::alphabet(s)
  expect_equivalent(a$letters, c("A", "C", "G", "U"))
  expect_equal(a$type, "RNA")
})

test_that("Alphabet DNA", {
  s = "ATGGCCAAT"
  a = alphabet(s)
  expect_equivalent(a$letters, c("A", "C", "G", "T"))
  expect_equal(a$type, "DNA")
})

test_that("Alphabet unknown", {
  s = "Hallo"
  a = alphabet(s)
  # Case is apparently ignored:
  expect_equivalent(a$letters, c("a", "H", "l", "o"))
  expect_equal(a$type, "unknown")
})