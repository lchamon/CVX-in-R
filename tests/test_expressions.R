# Source files
source('../utils.R')
source('../cvx.R')
source('../cvxfun.R')

# Load testthat
library(testthat)
context("CVX expressions")



### Start of test ###

test_that("Function with no definitions", {
  aff <- cvx(curvature = 'affine')
  conv <- cvx(curvature = 'convex')
  conc <- cvx(curvature = 'concave')
  
  f <- cvxfun(e1, e2)
  
  expect_error(f(conv, conv), 'has no ruleset')
  expect_error(f(conv, aff),  'has no ruleset')
  expect_error(f(10, 10),     'has no ruleset')
  expect_error(f(10, conc),   'has no ruleset')
  expect_error(f(a, conv),    'not found')
  expect_error(f(10),         'missing, with no default')
  expect_error(f(a),          'not found')
})


test_that("Arbitrary functions", {
  aff <- cvx(curvature = 'affine')
  conv <- cvx(curvature = 'convex')
  conc <- cvx(curvature = 'concave')
  
  g <- cvxfun(e1, e2) +
    dcprule(convex, convex, out = convex) +
    dcprule(convex, affine, out = convex) +
    dcprule(constant, concave, out = concave)
  
  h <- cvxfun(arg1, arg2) +
    dcprule(convex, convex, out = convex) +
    dcprule(convex, convex, out = affine) +
    dcprule(constant, convex, out = concave)
  
  j <- cvxfun(x) +
    dcprule(affine, out = convex) +
    dcprule(constant, out = constant)
  
  
  expect_is(g(conv, conv),                   'cvx')
  expect_equal(get_curvature(g(conv, conv)), 'convex')
  expect_is(g(conv, aff),                    'cvx')
  expect_equal(get_curvature(g(conv, aff)),  'convex')
  expect_is(g(10, conc),                     'cvx')
  expect_equal(get_curvature(g(10, conc)),   'concave')
  
  expect_error(g(aff, conv), '{affine}, {convex}',     fixed = TRUE)
  expect_error(g(10, 10),    '{constant}, {constant}', fixed = TRUE)  
  
  expect_is(h(10, conv),                   'cvx')
  expect_equal(get_curvature(h(10, conv)), 'concave')
  expect_error(h(conv, conv),              'inconsistent')
  expect_error(h(conc, conv),              '{concave}, {convex}', fixed = TRUE)
  
  expect_is(j(aff),                   'cvx')
  expect_equal(get_curvature(j(aff)), 'convex')
  expect_is(j(10),                    'cvx')
  expect_equal(get_curvature(j(10)),  'constant')
  expect_error(j(conv),               '{convex}', fixed = TRUE)
})
