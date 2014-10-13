## Built in CVX functions ##########################

## e1 + e2 ##########################
`+.cvx` <- function(e1, e2){
  # Ensure promises exist
  force(e1)
  force(e2)

  # Check dimensions
  if(any(dim(e1) != dim(e2))){
    stop('Dimensions are not compatible.')
  }
  
  # Check DCP rules
  curv <- dcpcheck(fname = '+', FUN = `+.cvx`, e1, e2)
  
  # Create addition object
  # Cannot trust R with scalar dimensions, so we only get dimensions from CVX objects
  if(is.cvx(e1)){
    cvx(dim(e1)[1], dim(e1)[2], curvature = curv)
  } else {
    cvx(dim(e2)[1], dim(e2)[2], curvature = curv)
  }
}

class(`+.cvx`) <- c("cvxfun")

`+.cvx` <- `+.cvx` +
  dcprule(convex,   convex,   out = convex) +
  dcprule(concave,  concave,  out = concave) +
  dcprule(affine,   affine,   out = affine) +
  dcprule(constant, constant, out = constant) +
  dcprule(constant, affine,   out = affine) +
  dcprule(affine,   constant, out = affine) +
  dcprule(constant, convex,   out = convex) +
  dcprule(convex,   constant, out = convex) +
  dcprule(constant, concave,  out = concave) +
  dcprule(concave,  constant, out = concave) +
  dcprule(affine,   convex,   out = convex) +
  dcprule(convex,   affine,   out = convex) +
  dcprule(affine,   concave,  out = concave) +
  dcprule(concave,  affine,   out = concave)



## e1 * e2 ##########################
`*.cvx` <- function(e1, e2){
  # Ensure promises exist
  force(e1)
  force(e2)
  
  # Check dimensions (any() solves the scalar case which have NULL dims)
  if(!is.scalar(e1) && !is.scalar(e2) && dim(e1)[2] != dim(e2)[1]){
    stop('Dimensions are not compatible.')
  }
  
  # Check DCP rules
  curv <- dcpcheck(fname = '*', FUN = `*.cvx`, e1, e2)
  
  # Create addition object
  # Cannot trust R with scalar dimensions, so we only get dimensions from CVX objects
  if (is.scalar(e1) && is.cvx(e2)){
    cvx(dim(e2)[1], dim(e2)[2], curvature = curv)
  } else if (is.scalar(e2) && is.cvx(e1)){
    cvx(dim(e1)[1], dim(e1)[2], curvature = curv)
  } else {
    cvx(dim(e1)[1], dim(e2)[2], curvature = curv)
  }
}

class(`*.cvx`) <- c("cvxfun")

`*.cvx` <- `*.cvx` +
  dcprule(x > 0 & constant, convex,           out = convex) +
  dcprule(x < 0 & constant, convex,           out = concave) +
  dcprule(x > 0 & constant, concave,          out = concave) +
  dcprule(x < 0 & constant, concave,          out = convex) +
  dcprule(constant,         affine,           out = affine) +
  dcprule(convex,           x > 0 & constant, out = convex) +
  dcprule(convex,           x < 0 & constant, out = concave) +
  dcprule(concave,          x > 0 & constant, out = concave) +
  dcprule(concave,          x < 0 & constant, out = convex) +
  dcprule(affine,           constant,         out = affine) +
  dcprule(constant,         constant,         out = constant)



## e1 - e2 ##########################
`-.cvx` <- function(e1, e2){
  e1 + (-1)*e2
}

class(`-.cvx`) <- c("cvxfun")



## x^p ##########################
`^.cvx` <- cvxfun(x, p)

`^.cvx` <- `^.cvx` +
  dcprule(affine & is.scalar(x),  constant & is.scalar(x) & x >= 1 & !(is.whole(x) & x %% 2 != 0 & x != 1), out = convex) +
  dcprule(concave & is.scalar(x), constant & is.scalar(x) & x > 0 & x < 1,                                  out = concave)


## norm(p, x) ##########################
norm <- cvxfun(x, p)
norm <- norm +
  dcprule(constant, constant & is.scalar(x) & x >= 1, out = constant) +
  dcprule(affine,   constant & is.scalar(x) & x >= 1, out = convex) +
  dcprule(convex,   constant & is.scalar(x) & x >= 1, out = convex)



## quad_over_lin(x, y) ##########################
quad_over_lin <- cvxfun(x, y)
quad_over_lin <- quad_over_lin +
  dcprule(affine,   concave  & is.scalar(x), out = convex) +
  dcprule(affine,   affine   & is.scalar(x), out = convex) +
  dcprule(constant, concave  & is.scalar(x), out = convex) +
  dcprule(constant, affine   & is.scalar(x), out = convex) +
  dcprule(affine,   constant & is.scalar(x) & x > 0, out = convex) +
  dcprule(constant, constant & is.scalar(x) & x > 0, out = constant)



