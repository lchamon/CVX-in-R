# Source files
source('../utils.R')
source('../cvx.R')
source('../cvxfun.R')
source('../cvx_builtins.R')

# Load testthat
library(testthat)
context("DCP rule set for norm()")

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
  expect_is(`norm`, 'cvxfun')
  expect_equal(length(formals(`norm`)), 2)
})


test_that("Valid operations", {
  affine <- cvx(10, 1, curvature = 'affine')
  convex <- cvx(10, 1, curvature = 'convex')
  constant <- cvx(10, 1, curvature = 'constant')
  
  for (p in seq(1,10,0.1)){
    expect_that(norm(constant, p),   is_constant())
    expect_that(norm(affine, 3*p+1), is_convex())
    expect_that(norm(convex, p+10),  is_convex())
  }
})


test_that("DCP violations", {
  affine <- cvx(curvature = 'affine')
  convex <- cvx(curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  
  for (p in seq(1,10,0.1)){
    expect_error(norm(concave, p), '{concave}, {constant}', fixed = TRUE)    
  }
  
  for (p in seq(1,10,0.1)){
    expect_error(norm(concave, p), '{concave}, {constant}', fixed = TRUE)    
  }
  
  expect_error(norm(15, convex), '{constant}, {convex}', fixed = TRUE)
  expect_error(norm(2, affine),  '{constant}, {affine}', fixed = TRUE)
  expect_error(norm(1, concave), '{constant}, {concave}', fixed = TRUE)
  
  expect_error(norm(affine, 0), '{affine}, {constant}', fixed = TRUE)
  expect_error(norm(10, 0),     '{constant}, {constant}', fixed = TRUE)
  expect_error(norm(convex, 0), '{convex}, {constant}', fixed = TRUE)
})
