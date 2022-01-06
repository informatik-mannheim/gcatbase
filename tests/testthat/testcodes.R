# Test file
library(testthat)

context("codes") # Infos

test_that("Code", {
  X = code(tuples = c("AUG", "GCG"), id = "A code")
  expect_equal(X$id, "A code")
})