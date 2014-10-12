# Source files
source('../aux_functions.R')
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
  
  expect_identical(class(f),     c('cvxfun', 'cvx'))
  expect_identical(class(curv1), c('curvature', 'cvx'))
  expect_identical(class(mono1), c('monotonicity', 'cvx'))
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


test_that("Check objects class", {
  x <- cvx()
  f <- cvxfun(e1, e2)
  curv1 <- curvature(convex)
  mono1 <- monotonicity(nondecreasing)
  
  expect_identical(get_curvature(f + curvature(convex)),                   curvature(convex))
  expect_identical(get_curvature(f + curvature('convex')),                 curvature(convex))
  expect_identical(get_curvature(f + curvature(affine)),                   curvature(affine))
  expect_identical(get_curvature(f + curv1),                               curvature(convex))
  expect_identical(get_curvature(f + curv1 + curvature(concave)),          curvature(concave))
  expect_identical(get_monotonicity(f + mono1),                           
                                                                  monotonicity(nondecreasing))
  expect_identical(get_monotonicity(f + monotonicity(nonincreasing)),     
                                                                  monotonicity(nonincreasing))
  expect_identical(get_monotonicity(f + mono1 + monotonicity(increasing)),
                                                                  monotonicity(increasing))
})
