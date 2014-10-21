# Source files
source('../utils.R')

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
  expect_false(is.scalar(c(1, 2, 3)))
  expect_error(is.scalar('a'),        'numeric value')
  expect_error(is.scalar(c('a','b')), 'numeric value')
})


test_that("is.whole", {
  x <- cvx()
  
  expect_true(is.whole(1))
  expect_true(is.whole(3))
  expect_true(is.whole(sqrt(2)/sqrt(2)))
  expect_true(is.whole(1 + .Machine$double.eps))
  expect_false(is.whole(1.2))
  expect_false(is.whole(1 + 2*.Machine$double.eps^0.5))
  
  expect_error(is.whole('a'), 'non-numeric argument')
  expect_error(is.whole(x),   'non-numeric argument')
})


test_that("subs_q", {
  expect_identical(subs_q(quote(a + b), env = list(a = 1, b = 2)), quote(1 + 2))
  expect_identical(subs_q(quote(a + b), env = list(a = 27)),       quote(27 + b))
  expect_identical(subs_q(quote(a + y), env = list(y = 10)),       quote(a + 10))
})
