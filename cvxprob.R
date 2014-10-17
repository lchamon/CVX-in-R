## CVX problems ###################################
minimize <- function(objective = 0){
  obj.out <- eval.parent(objective)
  
  if (!(get_curvature(obj.out) %in% c('convex', 'affine', 'constant'))) {
    stop('CVX only support minimizing convex objectives.')
  }
  
  if (!is.scalar(obj.out)) {
    stop('The objetive function must be a scalar. Consider scalarizing explicitly.')
  }
  
  structure(list(objective = substitute(objective),
                 constraints = NULL,
                 cones = NULL),
            class = 'cvxprob')
}


# is.cvxprob(): is a CVX function?
is.cvxprob <- function(x){
  inherits(x, 'cvxprob')
}


# objective(): get CVX problem objective
objective <- function(cvxprob){
  if (!is.cvxprob(cvxprob)){
    stop('The object is not a CVX problem.', call. = FALSE)
  }
  
  cvxprob$objective
}


# constraints(): get CVX problem constraints
constraints <- function(cvxprob){
  if (!is.cvxprob(cvxprob)){
    stop('The object is not a CVX problem.', call. = FALSE)
  }
  
  cvxprob$constraints
}


# cones(): get CVX problem cones constraints
cones <- function(cvxprob){
  if (!is.cvxprob(cvxprob)){
    stop('The object is not a CVX problem.', call. = FALSE)
  }
  
  cvxprob$cones
}


# print.cvxfun(): formatted printing of the definition of a CVX function
print.cvxprob <- function(cvxprob){
  cat("CVX problem")
  cat("\n")
  
  cat("minimize\t", deparse(objective(cvxprob)))
  cat("\n")
  
  if(!is.null(constraints(cvxprob))){
    cat("subject to")
    cat("\n")
    
    for (constraint in constraints(cvxprob)){
      cat('\t', deparse(constraint))
      cat("\n")
    }
  }
}


# +.cvxprob: adds attributes to CVX problems
`+.cvxprob` <- function(cvxprob, constraint){
  e2name <- deparse(substitute(constraint))
  
  # Add curvature
  if (inherits(constraint, 'cvx_constraint')) {
    cvxprob$constraints <- c(cvxprob$constraints, unclass(constraint))
    cvxprob
  }
  # Unsupported operation
  else stop("Don't know how to add ", e2name, " to a CVX problem.", call. = FALSE)
}


# subject_to(): create a CVX constraint
subject_to <- function(constraint) {
  structure(substitute(constraint), class = 'cvx_constraint')
}


# %st%: shortcut for adding constraints to CVX problems
# [TODO]
`%st%` <- function(cvxprob, constraint){
  stop('Empty function...')
}


## Canonical CVX problems ###################################
# canonical(): returns a canonicalized CVX problem
canonical <- function(cvxprob){
  # Return a canonical CVX problem
}


