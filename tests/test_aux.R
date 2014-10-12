# Source files
source('../aux_functions.R')

# Load testthat
library(testthat)
context("Auxiliary functions")


### Start of test ###

test_that("is.scalar", {
  x <- cvx()
  z <- cvx(10)
  b <- matrix(rnorm(10,0,2), nrow = 10)
  
  expect_true(is.scalar(1))
  expect_true(is.scalar(x))
  expect_false(is.scalar(z))
  expect_false(is.scalar(b))
  expect_false(is.scalar(t(b)))
  expect_error(is.scalar('a'), 'numeric value')
})


test_that("subs_q", {
  expect_identical(subs_q(quote(a + b), env = list(a = 1, b = 2)), quote(1 + 2))
  expect_identical(subs_q(quote(a + b), env = list(a = 27)),       quote(27 + b))
  expect_identical(subs_q(quote(a + y), env = list(y = 10)),       quote(a + 10))
})
