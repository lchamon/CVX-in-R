# Source files
source('../aux_functions.R')
source('../cvx.R')
source('../cvxfun.R')
source('../cvx_builtins.R')

# Load testthat
library(testthat)
context("DCP rule set for addition")

is_convex <- function(){
  function(x) {
    expectation(inherits(x, 'cvx') && get_curvature(x) == 'convex',
                paste0("is ", get_curvature(x), " instead of convex"),
                "is convex")
  }
}

is_concave <- function(){
  function(x) {
    expectation(inherits(x, 'cvx') && get_curvature(x) == 'concave',
                paste0("is ", get_curvature(x), " instead of concave"),
                "is concave")
  }
}

is_affine <- function(){
  function(x) {
    expectation(inherits(x, 'cvx') && get_curvature(x) == 'affine',
                paste0("is ", get_curvature(x), " instead of affine"),
                "is affine")
  }
}

is_constant <- function(){
  function(x) {
    expectation(inherits(x, 'cvx') && get_curvature(x) == 'constant',
                paste0("is ", get_curvature(x), " instead of constant"),
                "is constant")
  }
}



### Start of test ###

test_that("General properties", {
  expect_is(`+.cvx`, 'cvxfun')
  expect_equal(length(formals(`+.cvx`)), 2)
})


test_that("Same curvature", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  constant <- cvx(curvature = 'constant')
  k1 <- runif(1)
  
  expect_that(convex + convex + convex, is_convex())
  expect_that(concave + concave + concave, is_concave())
  expect_that(affine + affine + affine, is_affine())
  expect_equal(get_curvature(10 + 1 + 3), 'constant')
  expect_that(constant + 1 + k1, is_constant())
  expect_that(constant + k1, is_constant())
})


test_that("Add a constant", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  constant <- cvx(curvature = 'constant')
  k1 <- runif(1)
  
  expect_that(3 + convex + constant, is_convex())
  expect_that(1 + concave + constant, is_concave())
  expect_that(2 + affine + constant, is_affine())
  
  expect_that(convex + 2 + k1, is_convex())
  expect_that(concave + 2 + k1, is_concave())
  expect_that(affine + 2 + k1, is_affine())
  
  expect_that(-10 + convex + constant, is_convex())
  expect_that(-3 + concave + constant, is_concave())
  expect_that(-7 + affine + constant, is_affine())
  
  expect_that(k1 + convex + constant, is_convex())
  expect_that(k1 + concave + constant, is_concave())
  expect_that(k1 + affine + constant, is_affine())
  
  expect_that(convex + k1 + 2, is_convex())
  expect_that(concave + k1 + 7, is_concave())
  expect_that(affine + k1 + 9, is_affine())
  
  expect_that(convex + constant + k1, is_convex())
  expect_that(concave + constant + k1, is_concave())
  expect_that(affine + constant + k1, is_affine())
})


test_that("Add affine", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  constant <- cvx(curvature = 'constant')
  k1 <- runif(1)
  
  expect_that(3 + convex + affine, is_convex())
  expect_that(1 + concave + affine, is_concave())
  expect_that(2 + affine + affine, is_affine())
  
  expect_that(convex + affine, is_convex())
  expect_that(concave + affine, is_concave())
  
  expect_that(-10 + convex + affine, is_convex())
  expect_that(-3 + concave + affine, is_concave())
  expect_that(-7 + affine + affine, is_affine())
  
  expect_that(k1 + convex + affine, is_convex())
  expect_that(k1 + concave + affine, is_concave())
  expect_that(k1 + affine + affine, is_affine())
  
  expect_that(convex + constant + affine, is_convex())
  expect_that(concave + constant + affine, is_concave())
  expect_that(affine + constant + affine, is_affine())
})


test_that("DCP violations", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  constant <- cvx(curvature = 'constant')
  k1 <- runif(1)
  
  expect_error(convex + 10 + concave, '{convex}, {concave}', fixed = TRUE)
  expect_error(k1 + convex + concave, '{convex}, {concave}', fixed = TRUE)
  expect_error(convex + constant + convex + concave,  '{convex}, {concave}', fixed = TRUE)
})


test_that("Vector case", {
  # Skip for now, as no CVX objects are undimensional
  skip('Dimension checking is disabled.')
  
  affine <- cvx(10, 1, curvature = 'affine')
  convex <- cvx(10, 1, curvature = 'convex')
  concave <- cvx(10, 1, curvature = 'concave')
  constant <- cvx(10, 1, curvature = 'constant')
  affine_t <- cvx(1, 10, curvature = 'affine')
  convex_t <- cvx(1, 10, curvature = 'convex')
  concave_t <- cvx(1, 10, curvature = 'concave')
  constant_t <- cvx(1, 10, curvature = 'constant')
  k1 <- matrix(runif(10), 10, 1)
  
  expect_that(convex + convex,         is_convex())
  expect_equal(dim(convex + convex),   c(10,1))
  expect_that(convex + affine,         is_convex())
  expect_equal(dim(convex + affine),   c(10,1))
  expect_that(concave + concave,       is_concave())
  expect_equal(dim(concave + concave), c(10,1))
  expect_that(concave + affine,        is_concave())
  expect_equal(dim(concave + affine),  c(10,1))
  expect_that(convex + k1,             is_convex())
  expect_equal(dim(convex + k1),       c(10,1))
  expect_that(k1 + concave,            is_concave())
  expect_equal(dim(k1 + concave),      c(10,1))
  expect_that(convex + 3,              is_convex())
  expect_equal(dim(convex + 3),        c(10,1))
  expect_that(4 + concave,             is_concave())
  expect_equal(dim(4 + concave),       c(10,1))
  
  expect_that(convex_t + convex_t,         is_convex())
  expect_equal(dim(convex_t + convex_t),   c(1,10))
  expect_that(convex_t + affine_t,         is_convex())
  expect_equal(dim(convex_t + affine_t),   c(1,10))
  expect_that(concave_t + concave_t,       is_concave())
  expect_equal(dim(concave_t + concave_t), c(1,10))
  expect_that(concave_t + affine_t,        is_concave())
  expect_equal(dim(concave_t + affine_t),  c(1,10))
  expect_that(convex_t + t(k1),            is_convex())
  expect_equal(dim(convex_t + t(k1)),      c(1,10))
  expect_that(t(k1) + concave_t,           is_concave())
  expect_equal(dim(t(k1) + concave_t),     c(1,10))
  
  expect_error(affine + affine_t,   'Dimensions are not compatible')
  expect_error(affine_t + affine,   'Dimensions are not compatible')
  expect_error(convex + convex_t,   'Dimensions are not compatible')
  expect_error(convex_t + convex,   'Dimensions are not compatible')
  expect_error(concave + concave_t, 'Dimensions are not compatible')
  expect_error(concave_t + concave, 'Dimensions are not compatible')
  expect_error(concave_t + k1,      'Dimensions are not compatible')
  expect_error(convex_t + k1,       'Dimensions are not compatible')
})
