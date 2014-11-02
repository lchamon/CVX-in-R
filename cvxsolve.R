## Solving CVX problems ###################################

# solve(): solve a CVX problem
solve.cvxprob <- function(cvxprob) {
  if (!is.cvxprob(cvxprob)) {
    stop("Provide a CVX problem to be solved.", call. = FALSE)
  }
  
  ### Step 0: Canonicalize problem ###########
  cvxprob <- canonical(cvxprob)
  
  
  ### Step 1: Build problem matrix ###########
  #### Step 1.1: get problem variables
  # Get the environment of the CVX problem
  cvxenv <- get_environment(cvxprob)
  
  # Get the name of all variables in the problem
  vars <- get_vars(objective(cvxprob))
  vars <- c(vars, get_vars(constraints(cvxprob)))
  vars <- unique(vars)
  
  # Get dummy variables
  dummy <- sapply(vars, function(x) !exists(x, envir = cvxenv))
  dummy <- vars[dummy]
  
  # Get CVX objects
  cvxobj <- sapply(vars,
                   function(x)
                     exists(x, envir = cvxenv) && is.cvx(get(x, envir = cvxenv)))
  cvxobj <- vars[cvxobj]
  
  # All remaining variables are parameters of the problem
  pars <- setdiff(vars, c(dummy, cvxobj))
  
  
  #### Step 1.2: cones setup
  # Get cones
  cvx_cones <- cones(cvxprob)
  
  # Reorganize variables in the cones to be close to each other
  # Setup cone types and sizes
  cones_vars <- list()
  cones_sizes <- list()
  cones_types <- list()
  for (cone in cvx_cones) {
    if (length(cone[[2]]) > 1) {
      cones_vars <- c(cones_vars, lapply(as.list(cone[[2]][-1]), deparse))
      cones_sizes <- c(cones_sizes, length(as.list(cone[[2]][-1])))
      
      if (deparse(cone[[3]]) == 'lorentz()') {
        cones_types <- c(cones_types, 'q')
      }
      
    } else {
      cones_vars <- c(cones_vars, deparse(cone[[2]]))
      cones_sizes <- c(cones_sizes, 1)
      
      if (deparse(cone[[3]]) == 'nonnegative()') {
        cones_types <- c(cones_types, 'l')
      }
    }
  }
  
  # Add remaining variables
  vars_vector <- c(cones_vars, setdiff(c(dummy, cvxobj), unlist(cones_vars)))
  
  # Include remaining variables in cones [TODO]
  nvars <- length(setdiff(c(dummy, cvxobj), unlist(cones_vars)))
  cones_sizes <- c(cones_sizes, rep(1,nvars))
  cones_types <- c(cones_types, rep('l',nvars))
  
  
  #### Step 1.3: problem matrix setup
  
  
  
  #### Step 1.4: populate problem matrix
  
  
  
  
  ### Step 2: Prepare for solver (shim) ###########
  
  
  
  ### Step 3: Solve! ###########
  
  
  
  ### Step 4: Recover solution ###########
}



find_coef <- function(x, variable) {
    if (is.atomic(x) || is.name(x)) {
      # Atomic constant or named variable: TRUE if it is the variable we are looking for
      if (deparse(x) == variable) {
        TRUE
      } else {
        FALSE
      }
      
    } else if (is.call(x)) {
      # Call to function: call find_coef recursively
      vars <- unlist(lapply(as.list(x[-1]), find_coef, variable = variable))
      
      
      if (any(!sapply(vars, is.logical))) {
        coef <- sapply(vars, is.language) | sapply(vars, function(x) is.numeric(x) && x != 0)
        
        if (length(which(coef)) == 0) {
          return(0)
        } else if (length(which(coef)) > 1) {
          stop("There was an error, only one coefficient should be returned.")
        }
        
        if (identical(x[[1]], quote(`*`))) {
          # If it is a multiplication, then return whichever is multiplying the variable
          substitute(a*b, list(a = vars[which(!coef)],
                               b = vars[which(coef)]))
          
        } else if (identical(x[[1]], quote(`-`))) {
          # If it is a subtraction, then +1 or -1 depending on the position
          if (which(coef) == 2) {
            substitute(-a, list(a = vars[which(coef)][[1]]))
          } else {
            vars[which(coef)][[1]]
          }
          
        } else if (identical(x[[1]], quote(`+`))) {
          # If it is an addition, return +1
          vars[which(coef)][[1]]
          
        } else {
          stop('Wow! You should not have ', deparse(x[[1]]),
               ' in a canonicalized problem.')
        }
        
      } else if (any(vars)) {
        if (identical(x[[1]], quote(`*`))) {
          # If it is a multiplication, then return whichever is multiplying the variable
          x[[which(!vars) + 1]]
          
        } else if (identical(x[[1]], quote(`-`))) {
          # If it is a subtraction, then +1 or -1 depending on the position
          if (which(vars) == 2) {
            -1
          } else {
            +1
          }
          
        } else if (identical(x[[1]], quote(`+`))) {
          # If it is an addition, return +1
          +1
          
        } else {
          stop('Wow! You should not have ', deparse(x[[1]]),
               ' in a canonicalized problem.')
        }
        
      } else {
        0
      }
    
    } else if (is.pairlist(x)) {
      # Unsupported
      stop("Pairlists are not yet supported. Pass arguments by position instead of by name.")
      
    } else {
      # User supplied some incorrect input
      stop("Can't handle type ", typeof(x), call. = FALSE)
    }
}

