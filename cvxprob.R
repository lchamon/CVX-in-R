## CVX problems ###################################

# minimize(): create a minimization CVX problem
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
            class = 'cvxprob',
            type = 'minimization')
}


# is.cvxprob(): is a CVX function?
is.cvxprob <- function(x){
  inherits(x, 'cvxprob')
}


# type(): get the type of a CVX problem
type <- function(x){
  if (!is.cvxprob(x)) {
    stop('Can only retrieve the type of a CVX problem.')
  }
  
  attr(x, 'type')
}


# objective(): get CVX problem objective
objective <- function(cvxprob){
  if (!is.cvxprob(cvxprob)){
    stop('The object is not a CVX problem.', call. = FALSE)
  }
  
  cvxprob$objective
}


# objective() <- : set CVX problem objective (internal)
`objective<-` <- function(cvxprob, value) {
  if (is.numeric(value)) {
    obj <- value
  } else {
    obj <- substitute(value)
  }
  
  cvxprob$objective <- value
  cvxprob
}


# constraints(): get CVX problem constraints
constraints <- function(cvxprob){
  if (!is.cvxprob(cvxprob)){
    stop('The object is not a CVX problem.', call. = FALSE)
  }
  
  cvxprob$constraints
}


# constraints() <- : set CVX problem constraints (internal)
`constraints<-` <- function(cvxprob, value) {
  if (!is.list(value)) {
    stop('CVX problems constraints must be a list.')
  }
  
  cvxprob$constraints <- value
  cvxprob
}


# cones(): get CVX problem cones constraints
cones <- function(cvxprob){
  if (!is.cvxprob(cvxprob)){
    stop('The object is not a CVX problem.', call. = FALSE)
  }
  
  cvxprob$cones
}


# print.cvxfun(): formatted printing of the definition of a CVX function
print.cvxprob <- function(cvxprob, ...){
  cat("CVX problem")
  cat("\n")
  
  if (type(cvxprob) == 'minimization') {
    cat("minimize\t")
  } else if (type(cvxprob) == 'maximization') {
    cat("maximize\t")
  } else {
    stop(type(cvxprob), 'is not a supported CVX problem type.')
  }
  
  
  cat(deparse(objective(cvxprob)))
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
# `%st%` <- function(cvxprob, constraint){
#   stop('Empty function...')
# }
