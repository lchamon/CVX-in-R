# Source files
source('../utils.R')
source('../cvx.R')
source('../cvxfun.R')
source('../cvx_builtins.R')

# Load testthat
library(testthat)
context("DCP rule set for multiplication")

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
  expect_is(`*.cvx`, 'cvxfun')
  expect_equal(length(formals(`*.cvx`)), 2)
})


test_that("Product-free rule", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  
  expect_error(convex*convex,   '{convex}, {convex}',   fixed = TRUE)
  expect_error(concave*concave, '{concave}, {concave}', fixed = TRUE)
  expect_error(affine*affine,   '{affine}, {affine}',   fixed = TRUE)
  expect_error(convex*concave,  '{convex}, {concave}',  fixed = TRUE)
  expect_error(concave*convex,  '{concave}, {convex}',  fixed = TRUE)
  expect_error(convex*affine,   '{convex}, {affine}',   fixed = TRUE)
  expect_error(affine*convex,   '{affine}, {convex}',   fixed = TRUE)
  expect_error(concave*affine,  '{concave}, {affine}',  fixed = TRUE)
  expect_error(affine*concave,  '{affine}, {concave}',  fixed = TRUE)
})


test_that("Positive constant", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  k_pos <- runif(1)
  x <- 9
  
  expect_that(2*convex,      is_convex())
  expect_that(convex*3,      is_convex())
  expect_that(4*concave,     is_concave())
  expect_that(concave*7,     is_concave())
  expect_that(9*affine,      is_affine())
  expect_that(affine*3,      is_affine())
  expect_that(convex*x,      is_convex())
  expect_that(x*concave,     is_concave())
  expect_that(affine*x,      is_affine())
  expect_that(k_pos*convex,  is_convex())
  expect_that(concave*k_pos, is_concave())
  expect_that(k_pos*affine,  is_affine())
})


test_that("Negative constant", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  k_neg <- -runif(1)
  x <- -9
  
  expect_that(-2*convex,      is_concave())
  expect_that(convex*-3,      is_concave())
  expect_that(-4*concave,     is_convex())
  expect_that(concave*-7,     is_convex())
  expect_that(-9*affine,      is_affine())
  expect_that(affine*-3,      is_affine())
  expect_that(convex*x,       is_concave())
  expect_that(x*concave,      is_convex())
  expect_that(affine*x,       is_affine())
  expect_that(k_neg*convex,   is_concave())
  expect_that(concave*k_neg,  is_convex())
  expect_that(k_neg*affine,   is_affine())
})


test_that("Other DCP violations", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  constant <- cvx(curvature = 'constant')
  k1 <- runif(1)
  
  expect_error(0*convex, '{constant}, {convex}', fixed = TRUE)
  expect_error(0*concave, '{constant}, {concave}', fixed = TRUE)
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
  convex2 <- cvx(2, 1, curvature = 'convex')
  concave2 <- cvx(2, 1, curvature = 'concave')
  affine2 <- cvx(2, 1, curvature = 'affine')
  A <- matrix(runif(50), 5, 10)
  B <- matrix(runif(50), 10, 5)
  C <- matrix(c(1,-1,-2,3,-4,5, -4, 5), 4, 2)
  k1 <- matrix(runif(7), 1, 7)
  k2 <- matrix(runif(10), 1, 10)
  

  expect_that(A*convex,         is_convex())
  expect_equal(dim(A*convex),   c(5,1))
  expect_that(A*concave,        is_concave())
  expect_equal(dim(A*concave),  c(5,1))
  expect_that(A*affine,         is_affine())
  expect_equal(dim(A*affine),   c(5,1))
  
  expect_error(C*convex2,       '{constant}, {convex}', fixed = TRUE)
  expect_error(C*concave2,      '{constant}, {concave}', fixed = TRUE)
  expect_that(C*affine2,        is_affine())
  expect_equal(dim(C*affine2),  c(4,1))
  

  expect_that(-4*convex,        is_concave())
  expect_equal(dim(4*convex),   c(10,1))
  expect_that(-3*concave,       is_convex())
  expect_equal(dim(3*concave),  c(10,1))
  expect_that(-9*affine,        is_affine())
  expect_equal(dim(-9*affine),  c(10,1))

  expect_that(k2 * convex,        is_convex())
  expect_equal(dim(k2 * convex),  c(1,1))
  expect_that(k2 * concave,       is_concave())
  expect_equal(dim(k2 * concave), c(1,1))
  expect_that(k2 * affine,        is_affine())
  expect_equal(dim(k2 * affine),  c(1,1))

  expect_that(convex * k2,        is_convex())
  expect_equal(dim(convex * k2),  c(10,10))
  expect_that(concave * k2,       is_concave())
  expect_equal(dim(concave * k2), c(10,10))
  expect_that(affine * k2,        is_affine())
  expect_equal(dim(affine * k2),  c(10,10))

  expect_that(convex * k1,        is_convex())
  expect_equal(dim(convex * k1),  c(10,7))
  expect_that(concave * k1,       is_concave())
  expect_equal(dim(concave * k1), c(10,7))
  expect_that(affine * k1,        is_affine())
  expect_equal(dim(affine * k1),  c(10,7))

  expect_error(B*convex,    'Dimensions are not compatible')
  expect_error(B*concave,   'Dimensions are not compatible')
  expect_error(B*affine,    'Dimensions are not compatible')
  expect_error(B*convex_t,  'Dimensions are not compatible')
  expect_error(B*concave_t, 'Dimensions are not compatible')
  expect_error(B*affine_t,  'Dimensions are not compatible')
  expect_error(A*convex_t,  'Dimensions are not compatible')
  expect_error(A*concave_t, 'Dimensions are not compatible')
  expect_error(A*affine_t,  'Dimensions are not compatible')
  expect_error(k1*convex,   'Dimensions are not compatible')
  expect_error(k1*concave,  'Dimensions are not compatible')
  expect_error(k1*affine,   'Dimensions are not compatible')
})
