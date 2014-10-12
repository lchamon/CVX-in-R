# Source files
source('../aux_functions.R')
source('../cvx.R')

# Load testthat
library(testthat)
context("CVX objects")


### Start of test ###

test_that("Check objects class", {
  x <- cvx()
  m <- cvx(10, 5, curvature = 'concave')
  
  expect_identical(class(x), 'cvx')
  expect_identical(class(m), 'cvx')
})


test_that("is.cvx", {
  x <- cvx()
  y <- cvx(curvature = 'convex')
  z <- cvx(10, curvature = 'concave')
  
  expect_true(is.cvx(x))
  expect_true(is.cvx(y))
  expect_true(is.cvx(z))
  expect_false(is.cvx('x'))
  expect_false(is.cvx(1))
})

test_that("get_curvature", {
  x <- cvx()
  y <- cvx(curvature = 'convex')
  z <- cvx(10, curvature = 'concave')
  m <- cvx(10, 5, curvature = 'concave')
  
  expect_error(get_curvature('a'),      'Curvature is not defined')
  expect_error(get_curvature('convex'), 'Curvature is not defined')
  expect_identical(get_curvature(x),    'affine')
  expect_identical(get_curvature(y),    'convex')
  expect_identical(get_curvature(z),    'concave')
  expect_identical(get_curvature(2),    'constant')
})

test_that("dim.cvx", {
  x <- cvx()
  y <- cvx(curvature = 'convex')
  z <- cvx(10, curvature = 'concave')
  m <- cvx(10, 5, curvature = 'concave')
  
  expect_null(dim(2))
  expect_identical(dim(x), c(1,1))
  expect_identical(dim(y), c(1,1))
  expect_identical(dim(z), c(10,1))
  expect_identical(dim(m), c(10,5))
})
