# Source files
source('../utils.R')
source('../cvx.R')
source('../cvxfun.R')
source('../cvx_builtins.R')

# Load testthat
library(testthat)
context("DCP rule set for exponentiation")

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
  skip('Arbitrary exponentiation support has been suspended.')
  
  expect_is(`^.cvx`, 'cvxfun')
  expect_equal(length(formals(`^.cvx`)), 2)
})


test_that("Convex expressions", {
  skip('Arbitrary exponentiation support has been suspended.')
  
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  
  expect_that(affine^1, is_convex())

  for (p in seq(2.01,20,0.02)) {
    expect_that(affine^p, is_convex())
  }
  
  for (p in c(1, seq(2,2,20))) {
    expect_that(affine^p, is_convex())
  }
})


test_that("Concave expressions", {
  skip('Arbitrary exponentiation support has been suspended.')
  
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  
  expect_that(concave^0.1, is_concave())
  
  for (p in seq(0.01, 0.99, 0.01)){
    expect_that(concave^p, is_concave())
  }
})


test_that("DCP violations", {
  skip('Arbitrary exponentiation support has been suspended.')
  
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  
  expect_error(affine^0, '{affine}, {constant}', fixed = TRUE)
  
  for (p in seq(3,51,2)){
    expect_error(affine^p, '{affine}, {constant}', fixed = TRUE)
  }
  
  for (p in 0:10){
    expect_error(convex^p, '{convex}, {constant}', fixed = TRUE)
  }
  
  expect_error(concave^0, '{concave}, {constant}', fixed = TRUE)
  expect_error(concave^1, '{concave}, {constant}', fixed = TRUE)
  
  for (p in 2:10){
    expect_error(concave^p, '{concave}, {constant}', fixed = TRUE)
  }
})


test_that("Vector case", {
  skip('Arbitrary exponentiation support has been suspended.')
  
  affine <- cvx(10, 1, curvature = 'affine')
  convex <- cvx(10, 1, curvature = 'convex')
  concave <- cvx(10, 1, curvature = 'concave')
  constant <- cvx(10, 1, curvature = 'constant')
  A <- matrix(runif(3), 3, 1)
  
  expect_error(affine^2,    '{affine}, {constant}', fixed = TRUE)
  expect_error(affine^1,    '{affine}, {constant}', fixed = TRUE)
  expect_error(concave^0.5, '{concave}, {constant}', fixed = TRUE)
  
  expect_error(affine^c(1 ,2), '{affine}, {constant}', fixed = TRUE)
  expect_error(affine^A,       '{affine}, {constant}', fixed = TRUE)
  expect_error(concave^A,      '{concave}, {constant}', fixed = TRUE)
})
