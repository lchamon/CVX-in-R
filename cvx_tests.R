# Clean everything
rm(list = ls())

# Source files
source('cvx.R')
source('cvx_methods.R')
source('cvxfun.R')


# Test function
cvx_test <- function(call, result){
  ret <- tryCatch(eval.parent(call),
                  error = function(cond) 'CVX_ERROR')
  
  if (all(ret == result)){
    'CVX_PASS'
  } else {
    if(ret == 'CVX_ERROR') 'CVX_ERROR' else 'CVX_FAIL'
  }
}



# Report function
report <- function(x, name = 'TEST'){
  cat(name, ': ')
  
  if(all(x == 'CVX_PASS')){
    cat('PASS\n')
  } else {
    cat('FAIL\n')
    cat('\tFailed: ', which(x == 'CVX_FAIL'), '\n')
    cat('\tError: ', which(x == 'CVX_ERROR'), '\n')
  }
}




# Method tests
x <- cvx()
z <- cvx(10)
b <- matrix(rnorm(10,0,2), nrow = 10)

methods <- c(
  # is.scalar
  cvx_test(is.scalar('a'),            'CVX_ERROR'),
  cvx_test(is.scalar(1),              TRUE),
  cvx_test(is.scalar(x),              TRUE),
  cvx_test(is.scalar(z),              FALSE),
  cvx_test(is.scalar(b),              FALSE),
  cvx_test(is.scalar(t(b)),           FALSE),
  # subs_q
  cvx_test(quote(subs_q(quote(a + b),
           env = list(a = 1, b = 2))),  quote(1 + 2)),
  cvx_test(quote(subs_q(quote(a + b),
           env = list(a = 1))),         quote(1 + b)),
  cvx_test(quote(subs_q(quote(a + y),
           env = list(y = 10))),        quote(a + 10))
)

report(methods, name = 'METHODS')


# CVX tests
x <- cvx()
y <- cvx(curvature = 'convex')
z <- cvx(10, curvature = 'concave')
m <- cvx(10, 5, curvature = 'concave')

cvx_tests <- c(
  cvx_test(class(x),                'cvx'),
  cvx_test(class(m),                'cvx'),
  # is.cvx
  cvx_test(is.cvx(x),               TRUE),
  cvx_test(is.cvx(y),               TRUE),
  cvx_test(is.cvx(z),               TRUE),
  cvx_test(is.cvx('x'),             FALSE),
  cvx_test(is.cvx(1),               FALSE),
  # get_curvature
  cvx_test(get_curvature('a'),      'CVX_ERROR'),
  cvx_test(get_curvature('convex'), 'CVX_ERROR'),
  cvx_test(get_curvature(x),        'affine'),
  cvx_test(get_curvature(y),        'convex'),
  cvx_test(get_curvature(z),        'concave'),
  cvx_test(get_curvature(2),        'constant'),
  # dim
  cvx_test(is.null(dim(2)),         TRUE),
  cvx_test(dim(x),                  c(1,1)),
  cvx_test(dim(y),                  c(1,1)),
  cvx_test(dim(z),                  c(10,1)),
  cvx_test(dim(m),                  c(10,5))
)

report(cvx_tests, name = 'CVX')


# CVX function tests
x <- cvx()
f <- cvxfun(e1, e2)
curv1 <- curvature(convex)
mono1 <- monotonicity(nondecreasing)

cvxfun_tests <- c(
  cvx_test(class(f),                c('cvxfun', 'cvx')),
  cvx_test(class(curv1),            c('curvature', 'cvx')),
  cvx_test(class(mono1),            c('monotonicity', 'cvx')),
  # is.cvxfun
  cvx_test(is.cvxfun(f),            TRUE),
  cvx_test(is.cvxfun(x),            FALSE),
  cvx_test(is.cvxfun('x'),          FALSE),
  cvx_test(is.cvxfun(1),            FALSE),
  # get_curvature
  cvx_test(is.null(get_curvature(f)),                     TRUE),
  # + curvature
  cvx_test(get_curvature(f + curvature(convex)),          'convex'),
  cvx_test(get_curvature(f + curvature('convex')),        'convex'),
  cvx_test(get_curvature(f + curvature(affine)),          'affine'),
  cvx_test(get_curvature(f + curv1),                      'convex'),
  cvx_test(get_curvature(f + curv1 + curvature(concave)), 'concave'),
  # get_monotonicity
  cvx_test(is.null(get_monotonicity(f)),    TRUE),
  cvx_test(get_monotonicity('f'),           'CVX_ERROR'),
  cvx_test(get_monotonicity(1),             'CVX_ERROR'),
  cvx_test(get_monotonicity(x),             'CVX_ERROR'),
  # + monotonicity
  cvx_test(get_monotonicity(f + mono1),                             'nondecreasing'),
  cvx_test(get_monotonicity(f + monotonicity(nonincreasing)),       'nonincreasing'),
  cvx_test(get_monotonicity(f + mono1 + monotonicity(increasing)),  'increasing'),
  # get_ruleset
  cvx_test(is.null(get_ruleset(f)),    TRUE),
  cvx_test(get_ruleset('f'),          'CVX_ERROR'),
  cvx_test(get_ruleset(1),            'CVX_ERROR'),
  cvx_test(get_ruleset(x),            'CVX_ERROR')
)

report(cvxfun_tests, name = 'CVX FUNCTION DEFINITIONS')



# DCP rules tests
rule1 <- dcprule(convex, convex, convex)
rule2 <- dcprule(convex, affine, convex)
rule3 <- dcprule(constant, concave, concave)

dcprule_tests <- c(
  cvx_test(class(rule1),            c('dcprule', 'cvx')),
  cvx_test(class(rule2),            c('dcprule', 'cvx')),
  # dcprule
  cvx_test(quote(dcprule(affine, affine, affine)[[1]]),   quote(affine)),
  cvx_test(quote(dcprule(affine, affine, affine)[[2]]),   quote(affine)),
  cvx_test(quote(dcprule(affine, affine, affine)[[3]]),   quote(affine)),
  cvx_test(quote(rule1[[1]]),                             quote(convex)),
  cvx_test(quote(rule1[[2]]),                             quote(convex)),
  cvx_test(quote(rule1[[3]]),                             quote(convex)),
  cvx_test(quote(dcprule(convex, affine, convex)[[1]]),   rule2[[1]]),
  cvx_test(quote(dcprule(convex, affine, convex)[[2]]),   rule2[[2]]),
  cvx_test(quote(dcprule(convex, affine, convex)[[3]]),   rule2[[3]]),
  # ==.dcprule
  cvx_test(dcprule(convex, convex, convex) == rule1,      TRUE),
  cvx_test(rule2 == dcprule(convex, affine, convex),      TRUE),
  cvx_test(rule3 == rule3,                                TRUE),
  cvx_test(rule1 == rule3,                                FALSE),
  cvx_test(rule1 == list(quote(affine),quote(affine)),    'CVX_ERROR')
)

report(dcprule_tests, name = 'DCP RULES')



# Adding DCP rules to functions tests
rule1 <- dcprule(convex, convex, convex)
rule2 <- dcprule(convex, affine, convex)
rule3 <- dcprule(constant, concave, concave)
f <- cvxfun(e1, e2)
g <- cvxfun(e1, e2) + rule1 + rule2 + rule3
h <- cvxfun(e1, e2) + dcprule(convex, convex, convex) + dcprule(convex, convex, affine) + rule3

add_dcprule_tests <- c(
  cvx_test(class(f),                c('cvxfun', 'cvx')),
  cvx_test(class(g),                c('cvxfun', 'cvx')),
  cvx_test(class(h),                c('cvxfun', 'cvx')),
  # + dcprule
  cvx_test(get_ruleset(f + dcprule(affine, affine, affine))[[1]], 
           dcprule(affine, affine, affine)),
  cvx_test(get_ruleset(f + rule1)[[1]],                       dcprule(convex, convex, convex)),
  cvx_test(get_ruleset(f + rule1)[[1]],                       rule1),
  cvx_test(get_ruleset(f + rule1 + rule2)[[1]],               dcprule(convex, convex, convex)),
  cvx_test(get_ruleset(f + rule1 + rule2)[[2]],               dcprule(convex, affine, convex)),
  cvx_test(get_ruleset(f + rule1 +
                         dcprule(convex, affine, convex))[[1]],   rule1),
  cvx_test(get_ruleset(f + rule1 +
                         dcprule(convex, affine, convex))[[2]],   rule2),
  cvx_test(get_ruleset(g)[[1]],                                   rule1),
  cvx_test(get_ruleset(g)[[2]],                                   rule2),
  cvx_test(get_ruleset(g)[[3]],                                   rule3)
)

report(add_dcprule_tests, name = 'DCP RULESET CREATION')



# Checking DCP rules tests
aff <- cvx(curvature = 'affine')
conv <- cvx(curvature = 'convex')
conc <- cvx(curvature = 'concave')
rule1 <- dcprule(convex, convex, convex)
rule2 <- dcprule(convex, affine, convex)
rule3 <- dcprule(constant, concave, concave)
f <- cvxfun(e1, e2)
g <- cvxfun(e1, e2) + rule1 + rule2 + rule3
h <- cvxfun(e1, e2) + dcprule(convex, convex, convex) + dcprule(convex, convex, affine) + rule3

dcpcheck_tests <- c(
  # dcp_check_rule
  cvx_test(dcp_check_rule(rule1, conv, conv),       'convex'),
  cvx_test(dcp_check_rule(rule2, conv, aff),        'convex'),
  cvx_test(dcp_check_rule(rule1, conc, aff),        FALSE),
  cvx_test(dcp_check_rule(rule2, aff, conv),        FALSE),
  cvx_test(dcp_check_rule(rule3, 10, conc),         'concave'),
  cvx_test(dcp_check_rule(rule3, 10, conv),         FALSE),
  cvx_test(dcp_check_rule(rule1, aff, conv),        FALSE),
  cvx_test(dcp_check_rule(rule1, 'convex', conv),   'CVX_ERROR'),
  cvx_test(dcp_check_rule(rule3, 'a', conc),        'CVX_ERROR'),
  cvx_test(dcp_check_rule(rule3, conc, 10),         FALSE),
  cvx_test(dcp_check_rule(rule3, conc, 10),         FALSE),
  # dcpcheck
  cvx_test(dcpcheck(f, conv, conv), 'CVX_ERROR'),
  cvx_test(dcpcheck(f, conv, aff),  'CVX_ERROR'),
  cvx_test(dcpcheck(f, 10, 10),     'CVX_ERROR'),
  cvx_test(dcpcheck(f, 10, conc),   'CVX_ERROR'),
  cvx_test(dcpcheck(g, conv, aff),  'convex'),
  cvx_test(dcpcheck(g, aff, conv),  'CVX_ERROR'),
  cvx_test(dcpcheck(g, 10, conc),   'concave'),
  cvx_test(dcpcheck(g, 10, 10),     'CVX_ERROR'),
  cvx_test(dcpcheck(g, conv, conv), 'convex'),
  cvx_test(dcpcheck(h, conv, conv), 'CVX_ERROR'),
  cvx_test(dcpcheck(h, conc, conv), 'CVX_ERROR'),
  cvx_test(dcpcheck(h, conc, conv), 'CVX_ERROR'),
  cvx_test(dcpcheck(h, 10, conc),   'concave')
)

report(dcpcheck_tests, name = 'DCP RULES CHECK')
