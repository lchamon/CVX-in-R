## Solving CVX problems ###################################

# solve(): solve a CVX problem
solve <- function(cvxprob) {
  if (!is.cvxprob(cvxprob)) {
    stop("Provide a CVX problem to be solved.", call. = FALSE)
  }
  
  
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
  
  list(pars, dummy, cvxobj)
  
  
  #### Step 1.2: cones setup
  # Get the cones
  # Reorganize variables in the cones to be close to each other
  vars <- c(cvxobj, dummy)
  # Setup cone types and sizes
  
  
  #### Step 1.3: problem matrix setup
  
  
  #### Step 1.4: populate problem matrix
  
  
  
  
  ### Step 2: Prepare for solver (shim) ###########
  
  
  ### Step 3: Solve! ###########
  
  
  ### Step 4: Recover solution ###########
}


