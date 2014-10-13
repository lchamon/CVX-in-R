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
  expect_is(`-.cvx`, 'cvxfun')
  expect_equal(length(formals(`-.cvx`)), 2)
})


test_that("Opposite curvatures", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  
  expect_that(convex - concave,           is_convex())
  expect_that(concave - convex,           is_concave())
  expect_that(convex - concave - concave, is_convex())
  expect_that(concave - convex - convex,  is_concave())
  expect_that(concave - convex - affine,  is_concave())
  expect_that(convex - concave - affine,  is_convex())

})


test_that("Constants and affines", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  constant <- cvx(curvature = 'constant')
  k1 <- runif(1)
  
  expect_that(concave - 10 - affine,      is_concave())
  expect_that(convex - 10 - affine,       is_convex())
  expect_that(concave - 10 - convex,      is_concave())
  expect_that(convex - 10 - concave,      is_convex())
  expect_that(affine - affine - affine,   is_affine())
  expect_equal(get_curvature(10 - 9 - 3), 'constant')
  expect_that(constant - 1 - k1,          is_constant())
  expect_that(constant - k1,              is_constant())
})


test_that("DCP violations", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  constant <- cvx(curvature = 'constant')
  k1 <- runif(1)
  
  expect_error(convex - convex,                    '{convex}, {concave}', fixed = TRUE)
  expect_error(concave - concave,                  '{concave}, {convex}', fixed = TRUE)
  expect_error(k1 - convex + convex,               '{concave}, {convex}', fixed = TRUE)
  expect_error(convex - affine - convex - concave, '{convex}, {concave}', fixed = TRUE)
})


test_that("Vector case", {
  affine <- cvx(10, 1, curvature = 'affine')
  convex <- cvx(10, 1, curvature = 'convex')
  concave <- cvx(10, 1, curvature = 'concave')
  constant <- cvx(10, 1, curvature = 'constant')
  affine_t <- cvx(1, 10, curvature = 'affine')
  convex_t <- cvx(1, 10, curvature = 'convex')
  concave_t <- cvx(1, 10, curvature = 'concave')
  constant_t <- cvx(1, 10, curvature = 'constant')
  k1 <- matrix(runif(10), 10, 1)
  
  expect_that(convex - concave,       is_convex())
  expect_equal(dim(convex - concave), c(10,1))
  expect_that(convex - affine,        is_convex())
  expect_equal(dim(convex - affine),  c(10,1))
  expect_that(concave - convex,       is_concave())
  expect_equal(dim(concave - convex), c(10,1))
  expect_that(concave - affine,       is_concave())
  expect_equal(dim(concave - affine), c(10,1))
  expect_that(convex - k1,            is_convex())
  expect_equal(dim(convex - k1),      c(10,1))
  expect_that(k1 - concave,           is_convex())
  expect_equal(dim(k1 - concave),     c(10,1))
  expect_that(3 - convex,             is_concave())
  expect_equal(dim(3 - convex),       c(10,1))
  expect_that(concave - 4,            is_concave())
  expect_equal(dim(concave - 4),      c(10,1))
  
  expect_that(convex_t - concave_t,       is_convex())
  expect_equal(dim(convex_t - concave_t), c(1,10))
  expect_that(convex_t - affine_t,        is_convex())
  expect_equal(dim(convex_t - affine_t),  c(1,10))
  expect_that(concave_t - convex_t,       is_concave())
  expect_equal(dim(concave_t - convex_t), c(1,10))
  expect_that(concave_t - affine_t,       is_concave())
  expect_equal(dim(concave_t - affine_t), c(1,10))
  expect_that(convex_t - t(k1),           is_convex())
  expect_equal(dim(convex_t - t(k1)),     c(1,10))
  expect_that(t(k1) - concave_t,          is_convex())
  expect_equal(dim(t(k1) - concave_t),    c(1,10))
  
  expect_error(affine - affine_t,   'Dimensions are not compatible')
  expect_error(affine_t - affine,   'Dimensions are not compatible')
  expect_error(convex - convex_t,   'Dimensions are not compatible')
  expect_error(convex_t - convex,   'Dimensions are not compatible')
  expect_error(concave - concave_t, 'Dimensions are not compatible')
  expect_error(concave_t - concave, 'Dimensions are not compatible')
  expect_error(concave_t - k1,      'Dimensions are not compatible')
  expect_error(convex_t - k1,       'Dimensions are not compatible')
})
