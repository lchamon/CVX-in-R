# Source files
source('../aux_functions.R')
source('../cvx.R')
source('../cvxfun.R')
source('../cvx_builtins.R')
source('../cvxprob.R')

# Load testthat
library(testthat)
context("CVX problems")


### Start of test ###

test_that("Check objects class", {
  prob1 <- minimize()
  prob2 <- minimize() + subject_to(x >= 0)
  
  expect_is(prob1, 'cvxprob')
  expect_is(prob2, 'cvxprob')
})


test_that("is.cvxprob", {
  x <- cvx()
  prob1 <- minimize(x)
  prob2 <- minimize(2*x) + subject_to(x >= 0)
  feas <- minimize()
  
  expect_true(is.cvxprob(prob1))
  expect_true(is.cvxprob(prob2))
  expect_true(is.cvxprob(feas))
  expect_false(is.cvxprob(x))
  expect_false(is.cvxprob('prob1'))
  expect_false(is.cvxprob(1))
})

test_that("objective", {
  x <- cvx()
  y <- cvx()
  prob1 <- minimize(x + 2*y) + subject_to(x >= 0) + subject_to(4*y + 2 == 0)
  prob2 <- minimize(2*x + 3)
  feas <- minimize()
  
  expect_error(objective('a'),        'not a CVX problem')
  expect_error(objective(x),          'not a CVX problem')
  expect_error(objective('prob1'),    'not a CVX problem')
  expect_identical(objective(prob1), quote(x + 2*y))
  expect_identical(objective(prob2), quote(2*x + 3))
  expect_identical(objective(feas),  quote(0))
})

test_that("constraints", {
  x <- cvx()
  y <- cvx()
  prob1 <- minimize(x + 2*y) + subject_to(x >= 0) + subject_to(4*y + 2 == 0)
  prob2 <- minimize(2*x + 3) + subject_to(10*x <= 1)
  feas <- minimize() + subject_to(x <= 1) + subject_to(x >= 0)
  
  expect_error(constraints('a'), 'not a CVX problem')
  expect_error(constraints(x), 'not a CVX problem')
  expect_error(constraints('prob1'), 'not a CVX problem')
  expect_identical(constraints(prob1), list(quote(x >= 0), quote(4*y + 2 == 0)))
  expect_identical(constraints(prob2), list(quote(10*x <= 1)))
  expect_identical(constraints(feas), list(quote(x <= 1), quote(x >= 0)))
})

test_that("cones", {
  skip('Cones are not yet supported.')
})

