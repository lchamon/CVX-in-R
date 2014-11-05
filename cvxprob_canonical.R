## Canonical CVX problems ###################################
# Canonical convex problem
# minimize f(x)
# s.t. f(x) <= 0 ; Ax = b

# Canonical CVX problem
# minimize c'*x
# s.t. Ax = b
#      x >= 0


# canonical(): returns a canonicalized CVX problem
canonical <- function(cvxprob){
  if (!is.cvxprob(cvxprob)) {
    stop('Only CVX problems can be canonicalized.')
  }
  
  ### Step 1: Linearization (relaxed Smith form) ###########
  cvxprob <- linearize(cvxprob)
  
  ### Step 2: Graph implementations ###########
  cvxprob <- graph_implementation(cvxprob)
  
  ### Step 3: Canonicalize (in)equalities ###########
  #### Step 3.1: Add slack variable
  cvxprob <- add_slack(cvxprob)

  #### Step 3.2: Canonicalize RHS (Ax = b and x >= 0)
  cvxprob <- canonicalize_rhs(cvxprob)

  cvxprob
  
}


linearize <- function(cvxprob){
  #### Step 1: Linearize objetive
  lin.objective <- cvx_linearize(objective(cvxprob))
  
  #### Step 2: Linearize constraints
  if (length(constraints(cvxprob)) > 0) {
    # Linearize LHS and RHS of each constraint individually (using linearize) 
    # and then join them back (using cvx_linearize_affine). For set constraints,
    # do nothing!
    lin.constraints <- lapply(constraints(cvxprob),
                              function(constraint){
                                if (!identical(constraint[[1]], quote(`%in%`))) {
                                  cvx_linearize_affine(constraint[[1]],
                                                       lapply(constraint[-1], cvx_linearize))
                                } else {
                                  constraint
                                }
                              })

    # Join all results avoiding name clashes
    lin.constraints <- Reduce(function(x,y) c(x, shift_vars(y,
                                                            length(get_vars(x, 't\\d')),
                                                            reset = FALSE)),
                              lin.constraints[-1],
                              init = lin.constraints[[1]])
    
    # Rename to avoid name clashes with the new constraints from the
    # linearization of the objective
    lin.constraints <- shift_vars(lin.constraints,
                                  length(get_vars(lin.objective, 't\\d')),
                                  reset = FALSE)
    
  } else {
    # Unconstrained problem
    lin.constraints <- list()
  }
  
  #### Step 3: Construct linearized problem
  objective(cvxprob) <- lin.objective$FUN
  lin.objective$FUN <- NULL
  constraints(cvxprob) <- c(lin.objective,
                            unname(lin.constraints))
  
  cvxprob
}


# cvx_linearize(): linearize an expression by creating new dummy constraints
cvx_linearize <- function(x, envir = parent.frame()) {
  if (is.atomic(x)) {
    # Atomic constant: return value
    list(FUN = x)
    
  } else if (is.name(x)) {
    # Variable: return variable
    list(FUN = x)
    
  } else if (is.call(x)) {
    # Call to function: call cvx_linearize recursively
    vars <- lapply(as.list(x[-1]), cvx_linearize, envir = envir)
    
    # Check function call
    if (identical(x[[1]], quote(`+`)) |
          identical(x[[1]], quote(`-`)) |
          identical(x[[1]], quote(`*`))) {
      # Result is either constant or affine
      cvx_linearize_affine(x[[1]], vars)
      
    } else {
      ###### NONLINEAR ####################
      cvx_linearize_nonlinear(x[[1]], vars)
    }
    
  } else if (is.pairlist(x)) {
    # Unsupported for now
    stop("Pairlists are not yet supported. Pass arguments by position instead of by name.")
    
  } else {
    # User supplied some incorrect input
    stop("Can't handle type ", typeof(x), call. = FALSE)
  }
}


# cvx_linearize_affine(): linearize an affine expression
cvx_linearize_affine <- function(f, vars){
  # Update dummies names (if any) so that there is no clash during union
  vars <- Reduce(function(x,y) c(x, list(shift_vars(y,
                                                    length(get_vars(x, 't\\d')),
                                                    reset = FALSE))),
                 vars[-1],
                 init = list(vars[[1]]))
  
  # Form affine call
  out_call <- as.call(c(f, lapply(vars, `[[`, 'FUN')))
  
  # Get and join dummy variables
  dummies <- lapply(vars, function(x) { x[['FUN']] <- NULL; x })
  dummies <- Reduce(c, dummies)
  dummies[sapply(dummies, is.null)] <- NULL
  
  # Mount expression
  c(dummies, FUN = out_call)
}


# cvx_linearize_nonlinear(): linearize a nonlinear expression
cvx_linearize_nonlinear <- function(f, vars) {
  # Make nonlinearities atomic
  for (n in seq_along(vars)) {
    # Create a new dummy variable unless the argument
    # is a constant or a single variable
    if (length(vars[[n]]$FUN) > 1) {
      vars[[n]] <- shift_vars(vars[[n]], 1, reset = FALSE)
      vars[[n]] <- c(substitute(t1 == x, list(x = vars[[n]]$FUN)), vars[[n]])
      vars[[n]]$FUN <- quote(t1)
    }
    
    # If the argument is part of an inequality,
    # create new dummy equality constraint
    arg <- vars[[n]]$FUN
    
    if (!is.numeric(arg)) {
      dummies <- vars[[n]]
      dummies$FUN <- NULL
      
      # Check if the argument to the upper level function shows up in any of the
      # inequality constraints 
      repeated_var <- sapply(dummies,
                             function(x){
                               !identical(x[[1]], quote(`==`)) &&
                                 lapply(arg, get_vars) %in% get_vars(x)
                             })
      
      if (any(repeated_var)) {
        vars[[n]]$FUN <- shift_vars(list(arg), 1, reset = FALSE)[[1]]
        vars[[n]] <- c(substitute(told == tnew,
                                  list(told = arg, tnew = vars[[n]]$FUN)),
                       vars[[n]])
      }
    }

  }
  
  # Update dummies names so that there is no clash during union
  vars <- Reduce(function(x,y) c(x, list(shift_vars(y,
                                                    length(get_vars(x, 't\\d')),
                                                    reset = FALSE))),
                 vars[-1],
                 init = list(vars[[1]]))
  
  # Form nonlinear atom call
  out_call <- as.call(c(f, lapply(vars, `[[`, 'FUN')))
  
  # Get and join dummy variables
  dummies <- lapply(vars, function(x) { x[['FUN']] <- NULL; x })
  dummies <- Reduce(c, dummies)
  dummies[sapply(dummies, is.null)] <- NULL
  
  # Create a new epigraph variable for nonlinear atom
  epi_var <- paste0('t', length(get_vars(dummies, 't\\d'))+1)
  
  # Get the curvature of the function
  f_curvature <- get_curvature(match.fun(f))
  
  if (f_curvature == 'convex') {
    # Convex: f <= t
    out_call <- as.call(c(quote(`<=`), out_call, as.name(epi_var)))
    c(dummies, list(out_call), FUN = as.name(epi_var))
    
  } else if (f_curvature == 'concave') {
    # Concave: f >= t
    out_call <- as.call(c(quote(`>=`), out_call, as.name(epi_var)))
    c(dummies, list(out_call), FUN = as.name(epi_var))
    
  } else {
    stop("Can't linearize ", f_curvature, " functions.")
  }
}


# graph_implementation(): implement nonlinearities as epigraph CVX problems
graph_implementation <- function(cvxprob) {
  # Get problem constraints
  constraints <- constraints(cvxprob)
  
  # Auxiliary variable initializations
  N <- 0
  recurse <- TRUE
  
  while (recurse) {
    # Recurse over graph implementation?
    recurse <- FALSE
    new_cnstr <- list()
    
    for (n in seq_along(constraints)) {
      rel <- constraints[[n]][[1]]
      lhs <- constraints[[n]][[2]]
      rhs <- constraints[[n]][[3]]
      
      if (length(lhs) != 1 && is.call(lhs) && is.cvxfun(match.fun(lhs[[1]]))) {
        # Get epigraph
        epi <- get_epigraph(match.fun(lhs[[1]]))
        
        # Replace epi_var with RHS
        epi_var <- deparse(objective(epi))
        epi <- lapply(constraints(epi),
                      subs_q,
                      env = setNames(c(rhs, as.list(lhs[-1])),
                                     c(epi_var, names(formals(match.fun(lhs[[1]]))))))
        
        # Update variable names to avoid clashes
        epi <- shift_vars(epi, N, pattern = 'r\\d')
        N <- N + length(which(grepl('r\\d', unlist(lapply(epi, get_vars, pattern = 'r\\d')))))
        
        # Replace constraint
        new_cnstr <- c(new_cnstr, epi)
        
        # If a nonlinearity was replaced by its graph implementation,
        # recurse over it to see if another nonlinearity was used
        recurse <- TRUE
      } else {
        new_cnstr <- c(new_cnstr, constraints[[n]])
      }
    }
    
    constraints <- new_cnstr
  }
  
  constraints(cvxprob) <- constraints
  cvxprob
}


# [TODO] canonicalize_rhs(): canonicalize RHS of (in)equalities (A*x = b and x >= 0)
canonicalize_rhs <- function(cvxprob) {
  constraints <- constraints(cvxprob)
  if (length(constraints) > 0) {
    constraints(cvxprob) <- lapply(constraints, cvx_canonicalize_rhs)
  }
  
  # Eliminate NULL resulting from trivial constraints
  constraints(cvxprob)[sapply(constraints(cvxprob), is.null)] <- NULL
  
  cvxprob
}
  

# [TODO]
cvx_canonicalize_rhs <- function(constraint) {
  rel <- constraint[[1]]
  lhs <- constraint[[2]]
  rhs <- constraint[[3]]
  
  if (is.numeric(lhs) & is.numeric(rhs)) {
    # Check if constraint is trivially true...
    if (eval(constraint)) {
      NULL
    } else {
      stop("Constraint ", deparse(constraint), " cannot be satisfied.", call. = FALSE)
    }
    
  } else if (identical(rel, quote(`==`))) {
    # == pull everything to the left side [TODO]
    if (rhs == 0) {
      # Already canonical
      constraint
      
    } else if (lhs == 0) {
      # Almost canonical, just switch sides
      substitute(rhs == lhs,
                 list(rhs = rhs, lhs = lhs))
      
    } else {
      # Reverse sign of simplest side of the equation
      if (length(rhs) == 1) {
        substitute(lhs - rhs == 0,
                   list(lhs = lhs, rhs = rhs))
      } else if (length(lhs) == 1) {
        substitute(rhs - lhs == 0,
                   list(lhs = lhs, rhs = rhs))
      } else {
        stop('Complicated expressions are not yet supported. Try to rewrite constraint ',
             deparse(constraint), ' so that there is only one term on the LHS.')
      }
    }
    
  } else if (identical(rel, quote(`%in%`))) {
    # Do nothing
    constraint
    
  } else {
    # Should never get here!
    stop('Constraint cannot contain ', deparse(rel), '.')
  }
}


# [TODO]
add_slack <- function(cvxprob) {
  # Get constraints
  cvx_constraints <- constraints(cvxprob)
  
  # Count dummy variables in constraints
  nslack <- length(get_vars(cvx_constraints, 't\\d'))
  
  # Slack constraints
  slacks <- list()
  
  for (n in seq_along(cvx_constraints)){
    rel <- cvx_constraints[[n]][[1]]
    lhs <- cvx_constraints[[n]][[2]]
    rhs <- cvx_constraints[[n]][[3]]
    
    if (identical(rel, quote(`>=`))) {
      # New slack variable identifier
      nslack <- nslack + 1
      slack_name <- paste0('t', nslack)
      
      # Add slack variable
      cvx_constraints[[n]] <- substitute(lhs + s == rhs,
                                     list(lhs = lhs,
                                          rhs = rhs,
                                          s = as.name(slack_name)))

      slacks <- c(slacks,
                  substitute(s %in% nonnegative(), list(s = as.name(slack_name))))

    }
    
    if (identical(rel, quote(`<=`))) {
      # New slack variable identifier
      nslack <- nslack + 1
      slack_name <- paste0('t', nslack)
      
      # Add slack variable
      cvx_constraints[[n]] <- substitute(lhs - s == rhs,
                                     list(lhs = lhs,
                                          rhs = rhs,
                                          s = as.name(slack_name)))
      
      slacks <- c(slacks,
                  substitute(s %in% nonnegative(), list(s = as.name(slack_name))))
      
    }
  }

  constraints(cvxprob) <- c(cvx_constraints, slacks)
  cvxprob
}
