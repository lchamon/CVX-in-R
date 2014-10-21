## CVX class ###################################
# CVX object constructor
cvx <- function(rows = 1, cols = 1, curvature = 'affine', range = NULL) {
  if (!is.numeric(rows) | !is.numeric(cols)) {
    stop("Only numeric values are accepted")
  }
  
  if (length(rows) != 1 | length(cols) != 1) {
    stop("Dimensions must be scalar values")
  }
  
#   if (!(curvature %in% cvx_curvatures)){
#     stop("Curvature must be affine, convex, or concave")
#   }
  
  structure(NA,
            class = 'cvx',
            curvature = curvature,
            range = range,
            dimensions = c(rows, cols))
}


# is.cvx(): is a CVX object?
is.cvx <- function(x){
  inherits(x, 'cvx')
}


# print.cvx(): formatted description of CVX object
print.cvx <- function(x, ...){
  cat('CVX variable (', dim(x)[1], ' x ', dim(x)[2], ')')
  cat('\n')
  cat('Curvature:', get_curvature(x))
  cat('\n')
  
  cvxrange <- get_range(x)
  if (is.null(cvxrange)) {
    cat('Range:', '(-infty, +infty)')
  } else {
    cat('Range:', deparse(cvxrange))
  }
  cat('\n')
}


# dim.cvx(): returns dimensions of a CVX object
dim.cvx <- function(x){
  attr(x, 'dimensions')
}


# get_curvature(): returns the curvature of an object
get_curvature <- function(x) UseMethod('get_curvature')

get_curvature.default <- function(x) {
  if (is.numeric(x)) {
    # Numeric constants
    'constant'
  } else {
    stop('Curvature is not defined this object.')
  }
}

get_curvature.cvx <- function(x) {
  # CVX object
  attr(x, 'curvature')
}


# get_range(): returns the range of CVX objects
# TODO
get_range <- function(x) UseMethod('get_range')

get_range.default <- function(x) {
  stop('Range is only defined for CVX objects.')
}

get_range.cvx <- function(x) {
  range <- attr(x, 'range')
}
