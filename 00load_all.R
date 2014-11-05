rm(list = ls())

# Load solver
library(CLSOCP)

# Load all (in order)
source('utils.R')
source('cvx.R')
source('cvxfun.R')
source('cvxset.R')
source('cvxprob.R')
source('cvx_builtins.R')
source('cvxprob_canonical.R')
source('cvxsolve.R')
