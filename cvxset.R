## CVX sets ###################################
# is.cvxset(): is a CVX set?
is.cvxset <- function(x){
  inherits(x, 'cvxset')
}


# print.cvxset(): formatted description of a VX set
print.cvxset <- function(x, ...){
  cat('CVX set')
  cat('\n')
  if (is.name(x)){
    # Variable in set
    cat(deparse(x), 'in', get_type(x), '(', dim(x), ')')
    
  } else {
    # Clear set
    cat(get_type(x), '(', dim(x), ')')
  }
  cat('\n')
}


# dim.cvxset(): returns dimensions of a CVX set
dim.cvxset <- function(x){
  attr(x, 'dimensions')
}


# get_type(): returns the type of a set
get_type <- function(x) {
  if (!is.cvxset(x)){
    stop('Input is not a CVX set.')
  }
  
  attr(x, 'type')
}


# %in% operator
`%in%` <- function(x, table) UseMethod('%in%', table)


`%in%.default` <- function(x, table) {
  base::`%in%`(x, table)
}


`%in%.cvxset` <- function(x, set) {
  set_attr <- attributes(set)
  
  set <- substitute(x)
  attributes(set) <- set_attr
  
  set
}


# c(): concatenate vectors for sets
c.cvx <- function(...) {
  out <- list(...)
  
  if (length(out) == 1) {
    out <- substitute(...)
  }
  
  out
}


## Built in sets ###################################
# Nonnegative cone
nonnegative <- function(dim = NA) {
#   if (!is.scalar(dim)){
#     stop('The dimension of the set must be a scalar.')
#   }
  
  structure(NA,
            class = 'cvxset',
            type = 'nonnegative',
            dimensions = dim)
}


# Second-order cone (Lorentz cone)
lorentz <- function(dim = NA) {
#   if (!is.scalar(dim)){
#     stop('The dimension of the set must be a scalar.')
#   }

  structure(NA,
            class = 'cvxset',
            type = 'lorentz',
            dimensions = dim)
}
