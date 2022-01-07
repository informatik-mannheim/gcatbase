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

context("tuples creation")

# Syntax: alphabet size x tuple size

test_that("1 x 1", {
  t = all_tuples(1, c("X"))
  expect_equal(t, "X")
})

test_that("1 x 2", {
  t = all_tuples(2, c("X"))
  expect_equal(t, "XX")
})

test_that("2 x 1", {
  t = all_tuples(1, c("X", "Y"))
  expect_equal(t, c("X", "Y"))
})

context("tuples normalize")

test_that("Upperase no change", {
  s = normalize("ATGGCAG")
  expect_equal(s, "ATGGCAG")
})

test_that("Upperase U->T", {
  s = normalize("AUGGCAG")
  expect_equal(s, "ATGGCAG")
})

test_that("Lowercase t-U", {
  s = normalize("auggcag")
  expect_equal(s, "ATGGCAG")
})

test_that("To Lowercase DNA", {
  s = normalize("ATGGCAG", lowercase = T)
  expect_equal(s, "atggcag")
})

test_that("To Lowercase RNA", {
  s = normalize("AUGGCAG", RNA = T, lowercase = T)
  expect_equal(s, "auggcag")
})

test_that("Nonsense sequence", {
  s = normalize("Hello Mouse!")
  expect_equal(s, "HELLO MOTSE!")
})


