# Source files
source('../utils.R')
source('../cvx.R')
source('../cvxfun.R')

# Load testthat
library(testthat)
context("CVX functions")


### Start of test ###

test_that("Check classes", {
  f <- cvxfun(e1, e2)
  curv1 <- curvature(convex)
  mono1 <- monotonicity(nondecreasing)
  
  expect_is(f,     'cvxfun')
  expect_is(curv1, 'cvx_curvature')
  expect_is(mono1, 'cvx_monotonicity')
})


test_that("is.cvxfun", {
  x <- cvx()
  f <- cvxfun(e1, e2)
  
  expect_true(is.cvxfun(f))
  expect_false(is.cvxfun(x))
  expect_false(is.cvxfun('f'))
  expect_false(is.cvxfun(1))
})


test_that("get methods for CVX functions", {
  x <- cvx()
  f <- cvxfun(e1, e2)
  curv1 <- curvature(convex)
  mono1 <- monotonicity(nondecreasing)
  
  expect_null(get_curvature(f))
  expect_null(get_monotonicity(f))
  expect_error(get_monotonicity('f'), 'Monotonicity is only defined')
  expect_error(get_monotonicity(1),   'Monotonicity is only defined')
  expect_error(get_monotonicity(x),   'Monotonicity is only defined')
  expect_null(get_ruleset(f))
  expect_error(get_ruleset('f'), 'DCP rules are only defined')
  expect_error(get_ruleset(1),   'DCP rules are only defined')
  expect_error(get_ruleset(x),   'DCP rules are only defined')
})


test_that("Setting CVX functions atrributes", {
  x <- cvx()
  f <- cvxfun(e1, e2)
  curv1 <- curvature(convex)
  mono1 <- monotonicity(nondecreasing)
  
  expect_equal(get_curvature(f + curvature(convex)),                   'convex')
  expect_equal(get_curvature(f + curvature('convex')),                 'convex')
  expect_equal(get_curvature(f + curvature(affine)),                   'affine')
  expect_equal(get_curvature(f + curv1),                               'convex')
  expect_equal(get_curvature(f + curv1 + curvature(concave)),          'concave')
  expect_equal(get_monotonicity(f + mono1),                            'nondecreasing')
  expect_equal(get_monotonicity(f + monotonicity(nonincreasing)),      'nonincreasing')
  expect_equal(get_monotonicity(f + mono1 + monotonicity(increasing)), 'increasing')
})
