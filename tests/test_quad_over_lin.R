# Source files
source('../aux_functions.R')
source('../cvx.R')
source('../cvxfun.R')
source('../cvx_builtins.R')

# Load testthat
library(testthat)
context("DCP rule set for quad_over_lin()")

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
  expect_is(`quad_over_lin`, 'cvxfun')
  expect_equal(length(formals(`quad_over_lin`)), 2)
})


test_that("Valid operations", {
  affine <- cvx(10, 1, curvature = 'affine')
  affine1 <- cvx(curvature = 'affine')
  convex <- cvx(10, 1, curvature = 'convex')
  concave <- cvx(curvature = 'concave')
  constant <- cvx(10, 1, curvature = 'constant')
  
  expect_that(quad_over_lin(affine, concave),   is_convex())
  expect_that(quad_over_lin(affine, affine1),    is_convex())
  expect_that(quad_over_lin(constant, concave), is_convex())
  expect_that(quad_over_lin(constant, affine1),  is_convex())
  expect_that(quad_over_lin(affine, 2),         is_convex())
  expect_that(quad_over_lin(constant, 10),      is_constant())
})


test_that("DCP violations", {
  affine <- cvx(10, 1, curvature = 'affine')
  convex <- cvx(10, 1, curvature = 'convex')
  convex1 <- cvx(curvature = 'convex')
  concave <- cvx(10, 1, curvature = 'concave')
  concave1 <- cvx(curvature = 'concave')
  constant <- cvx(10, 1, curvature = 'constant')
  
  expect_error(norm(affine, constant), '{affine}, {constant}', fixed = TRUE)
  expect_error(norm(affine, c(1,0)),   '{affine}, {constant}', fixed = TRUE)
  expect_error(norm(concave, 2),       '{concave}, {constant}', fixed = TRUE)
  expect_error(norm(affine, 0),        '{affine}, {constant}', fixed = TRUE)
  expect_error(norm(affine, convex1),  '{affine}, {convex}', fixed = TRUE)
  expect_error(norm(convex, concave1), '{convex}, {concave}', fixed = TRUE)
})
