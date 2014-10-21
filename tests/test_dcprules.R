# Source files
source('../utils.R')
source('../cvx.R')
source('../cvxfun.R')

# Load testthat
library(testthat)
context("DCP rules")



### Start of test ###

test_that("Check classes", {
  rule1 <- dcprule(convex, convex, out = convex)
  rule2 <- dcprule(convex, affine, out = convex)
  
  expect_is(rule1, c('dcprule', 'cvx'))
  expect_is(rule2, c('dcprule', 'cvx'))
})


test_that("DCP rule declaration", {
  rule1 <- dcprule(convex, convex, out = convex)
  rule2 <- dcprule(convex, affine, out = convex)
  rule3 <- dcprule(constant, concave, out = concave)
  
  expect_identical(dcprule(affine, affine, out = affine)[[1]], quote(affine))
  expect_identical(dcprule(affine, affine, out = affine)[[2]], quote(affine))
  expect_identical(dcprule(affine, affine, out = affine)[[3]], quote(affine))
  expect_identical(rule1[[1]],                                 quote(convex))
  expect_identical(rule1[[2]],                                 quote(convex))
  expect_identical(rule1[[3]],                                 quote(convex))
  expect_identical(dcprule(convex, affine, out = convex)[[1]], rule2[[1]])
  expect_identical(dcprule(convex, affine, out = convex)[[2]], rule2[[2]])
  expect_identical(dcprule(convex, affine, out = convex)[[3]], rule2[[3]])
  expect_identical(dcprule(affine, out = convex)[[1]],         quote(affine))
  expect_identical(dcprule(affine, out = convex)[[2]],         quote(convex))
  expect_error(dcprule(affine, convex),                        'provide an outcome')
  expect_error(dcprule(affine, convex, convex),                'provide an outcome')
})


test_that("DCP rule comparison", {
  rule1 <- dcprule(convex, convex, out = convex)
  rule2 <- dcprule(convex, affine, out = convex)
  rule3 <- dcprule(constant, concave, out = concave)
  
  expect_true(dcprule(convex, convex, out = convex) == rule1)
  expect_true(rule2 == dcprule(convex, affine, out = convex))
  expect_true(rule3 == rule3)
  expect_false(rule1 == rule3)
  expect_error(rule1 == list(quote(affine), quote(affine)), 'is.dcprule\\(r2\\)')
  expect_error(rule1 == dcprule(convex, out = concave),     'must have the same length')
})


test_that("Adding DCP rules to functions", {
  rule1 <- dcprule(convex, convex, out = convex)
  rule2 <- dcprule(convex, affine, out = convex)
  rule3 <- dcprule(constant, concave, out = concave)
  f <- cvxfun(e1, e2)
  g <- cvxfun(e1, e2) + rule1 + rule2 + rule3
  h <- cvxfun(e1, e2) +
    dcprule(convex, convex, out = convex) +
    dcprule(convex, convex, out = affine) +
    rule3
  j <- cvxfun(e1)
  
  expect_is(f, c('cvxfun', 'cvx'))
  expect_is(g, c('cvxfun', 'cvx'))
  expect_is(h, c('cvxfun', 'cvx'))
  
  expect_identical(get_ruleset(f + dcprule(affine, affine, out = affine))[[1]],
                      dcprule(affine, affine, out = affine))
  expect_identical(get_ruleset(f + rule1 + dcprule(convex, affine, out = convex))[[1]], rule1)
  expect_identical(get_ruleset(f + rule1 + dcprule(convex, affine, out = convex))[[2]], rule2)
  expect_identical(get_ruleset(f + rule1)[[1]], dcprule(convex, convex, out = convex))
  expect_identical(get_ruleset(f + rule1)[[1]], rule1)
  expect_identical(get_ruleset(f + rule1 + rule2)[[1]], dcprule(convex, convex, out = convex))
  expect_identical(get_ruleset(f + rule1 + rule2)[[2]], dcprule(convex, affine, out = convex))
  expect_error(f + rule1 + dcprule(convex, affine, convex), 'provide an outcome')
  expect_identical(get_ruleset(g)[[1]], rule1)
  expect_identical(get_ruleset(g)[[2]], rule2)
  expect_identical(get_ruleset(g)[[3]], rule3)
  expect_error(f + dcprule(convex, out = convex), 'number of arguments')
  expect_identical(get_ruleset(j + dcprule(convex, out = convex))[[1]],
                      dcprule(convex, out = convex))
  expect_error(get_ruleset(j + rule1), 'number of arguments')
  expect_error(get_ruleset(j + rule2), 'number of arguments')
})


test_that("Simple DCP rules check", {
  aff <- cvx(curvature = 'affine')
  conv <- cvx(curvature = 'convex')
  conc <- cvx(curvature = 'concave')
  
  rule1 <- dcprule(convex, convex, out = convex)
  rule2 <- dcprule(convex, affine, out = convex)
  rule3 <- dcprule(constant, concave, out = concave)
  
  expect_equal(dcp_check_rule(rule1, list(conv, conv)), 'convex')
  expect_equal(dcp_check_rule(rule2, list(conv, aff)),  'convex')
  expect_equal(dcp_check_rule(rule3, list(10, conc)),   'concave')
  expect_false(dcp_check_rule(rule1, list(conc, aff)))
  expect_false(dcp_check_rule(rule2, list(aff, conv)))
  expect_false(dcp_check_rule(rule3, list(10, conv)))
  expect_false(dcp_check_rule(rule1, list(aff, conv)))
  expect_false(dcp_check_rule(rule3, list(conc, 10)))
  expect_false(dcp_check_rule(rule3, list(conc, 10)))
  expect_error(dcp_check_rule(rule1, list('convex', conv)), 'Curvature is not defined')
  expect_error(dcp_check_rule(rule3, list('a', conc)),      'Curvature is not defined')
  expect_error(dcp_check_rule(rule3, list(aff)),            'number of arguments')
  expect_error(dcp_check_rule(conv, list('a', conc)),       'provide a DCP rule')
})


test_that("Composed DCP rules check", {
  aff <- cvx(curvature = 'affine')
  conv <- cvx(curvature = 'convex')
  conc <- cvx(curvature = 'concave')
  
  rule1 <- dcprule(x > 0 && constant, convex, out = convex)
  rule2 <- dcprule(x < 0 && constant, convex, out = concave)
  rule3 <- dcprule(x < 0 && constant, affine, out = affine)
  rule4 <- dcprule(x < 0 && constant, concave, out = convex)
  
  expect_equal(dcp_check_rule(rule1, list(10, conv)), 'convex')
  expect_equal(dcp_check_rule(rule2, list(-1, conv)), 'concave')
  expect_equal(dcp_check_rule(rule3, list(-2, aff)),  'affine')
  expect_equal(dcp_check_rule(rule4, list(-2, conc)), 'convex')
  expect_false(dcp_check_rule(rule3, list(-2, conv)))
  expect_false(dcp_check_rule(rule3, list(conc, 10)))
})


test_that("Function DCP rules check", {
  aff <- cvx(curvature = 'affine')
  conv <- cvx(curvature = 'convex')
  conc <- cvx(curvature = 'concave')
  rule1 <- dcprule(convex, convex, out = convex)
  rule2 <- dcprule(convex, affine, out = convex)
  rule3 <- dcprule(constant, concave, out = concave)
  f <- cvxfun(e1, e2)
  g <- cvxfun(e1, e2) + rule1 + rule2 + rule3
  h <- cvxfun(e1, e2) +
    dcprule(convex, convex, out = convex) +
    dcprule(convex, convex, out = affine) +
    rule3
  j <- cvxfun(e1) + dcprule(affine, out = convex) + dcprule(constant, out = constant)
  l <- cvxfun(x, y) +
    dcprule(x > 0 && constant, concave, out = concave) +
    dcprule(x < 0 && constant, concave, out = convex)
  x <- -9
  wrong <- cvxfun(x, y) + dcprule(y > 0 && constant, concave, out = concave)
  
  expect_error(dcpcheck(FUN = f, fname = 'f', conv, conv), 'has no ruleset')
  expect_error(dcpcheck(FUN = f, fname = 'f', conv, aff),  'has no ruleset')
  expect_error(dcpcheck(FUN = f, fname = 'f', 10, 10),     'has no ruleset')
  expect_error(dcpcheck(FUN = f, fname = 'f', 10, conc),   'has no ruleset')
  expect_equal(dcpcheck(FUN = g, fname = 'g', conv, aff),  'convex')
  expect_equal(dcpcheck(FUN = g, fname = 'g', 10, conc),   'concave')
  expect_equal(dcpcheck(FUN = g, fname = 'g', conv, conv), 'convex')
  expect_error(dcpcheck(FUN = g, fname = 'g', aff, conv),  '{affine}, {convex}', fixed = TRUE)
  expect_error(dcpcheck(FUN = g, fname = 'g', 10, 10),     '{constant}, {constant}', fixed = TRUE)
  expect_equal(dcpcheck(FUN = h, fname = 'h', 10, conc),   'concave')
  expect_error(dcpcheck(FUN = h, fname = 'h', conv, conv), 'inconsistent')
  expect_error(dcpcheck(FUN = h, fname = 'h', conc, conv), '{concave}, {convex}', fixed = TRUE)
  expect_equal(dcpcheck(FUN = j, fname = 'j', 10),         'constant')
  expect_equal(dcpcheck(FUN = j, fname = 'j', aff),        'convex')
  expect_error(dcpcheck(FUN = j, fname = 'j', conv),       '{convex}', fixed = TRUE)
  expect_equal(dcpcheck(FUN = l, fname = 'l', 1, conc),    'concave')
  expect_equal(dcpcheck(FUN = l, fname = 'l', -3, conc),   'convex')
  expect_equal(dcpcheck(FUN = l, fname = 'l', x, conc),    'convex')
  expect_equal(dcpcheck(FUN = l, fname = 'l', -x, conc),   'concave')
  expect_error(dcpcheck(FUN = l, fname = 'l', -3, conv),   '{constant}, {convex}', fixed = TRUE)
  # [TODO] bad error message
  expect_error(dcpcheck(FUN = wrong, fname = 'wrong', -3, conv), 'not found')
})






