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
  
  structure(NULL,
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
print.cvx <- function(x){
  cat('CVX variable (', dim(x)[1], ' x ', dim(x)[2], ')')
  cat('\n')
  cat('Curvature:', get_curvature(x))
  cat('\n')
  cat('Range:', get_range(x))
  cat('\n')
}


# dim.cvx(): returns dimensions of a CVX object
dim.cvx <- function(x){
  attr(x, 'dimensions')
}


# get_curvature(): returns the curvature of an object
get_curvature <- function(x) {
  if (is.numeric(x)) {
    # Numeric constants
    'constant'
  } else if (is.cvx(x)) {
    # CVX object
    attr(x, 'curvature')
  } else {
    stop('Curvature is not defined this object')
  }
}


# get_range(): returns the range of CVX objects
# TODO
get_range <- function(x) {
  if (is.cvx(x)){
    range <- attr(x, 'range')
    
    if (is.null(range)) {
      '(-infty, +infty)'
    } else {
      deparse(range)
    }
  } else {
    stop('Range is only defined for CVX objects.')
  }
}
