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
  
  #### Step 1.1: Linearize objetive
  lin.objective <- linearize(objective(cvxprob))
  
  #### Step 1.2: Linearize constraints
  if (!is.null(constraints(cvxprob))) {
    # Linearize LHS and RHS of each constraint individually (using linearize)
    # and then join them back (using linearize_affine)
    lin.constraints <- lapply(constraints(cvxprob),
                              function(constraint){
                                linearize_affine(constraint[[1]],
                                                 lapply(constraint[-1], linearize))
                              })
    
    # Join all results avoiding name clashes
    lin.constraints <- Reduce(function(x,y) c(x, shift_varnames(y, sum(sapply(x,length)-1))),
                              lin.constraints[-1],
                              init = lin.constraints[[1]])
    
    # Rename to avoid name clashes with the new constraints
    # from the linearization of the objective
    lin.constraints <- shift_varnames(lin.constraints, length(lin.objective)-1)
    
  } else {
    # Unconstrained problem
    lin.constraints <- list()
  }
  
  #### Step 1.3: Construct linearized problem
  objective(cvxprob) <- lin.objective$FUN
  constraints(cvxprob) <- c(lin.objective[setdiff(names(lin.objective), 'FUN')],
                            unname(lin.constraints))
  
  
  ### Step 2: Canonicalize inequalities ###########
  #### Step 2.1: Canonicalize RHS
  # Ax = b and x >= 0
  constraints(cvxprob) <- lapply(constraints(cvxprob), canonicalize_rhs)
  
  #### Step 2.2: Add slack variables
  constraints(cvxprob) <- add_slack(constraints(cvxprob))
  
  cvxprob
  
  ### Step 3: Graph implementations of nonlinearities ###########
  
}

# exp <- cvxfun(x) + curvature(convex) + dcprule(affine, out = convex)
# log <- cvxfun(x) + curvature(concave) + dcprule(concave, out = concave)
# sqrt <- cvxfun(x) + curvature(concave) + dcprule(affine, out = concave)
# prob <- minimize() + subject_to(exp(y) <= log(2*sqrt(x)+1)) + subject_to(A*x == b)
# canonical(prob)


# linearize(): linearize an expression by creating new dummy constraints
linearize <- function(x, envir = parent.frame()) {
  if (is.atomic(x)) {
    # Atomic constant: return value
    list(FUN = x)
    
  } else if (is.name(x)) {
    # Variable: return variable
    list(FUN = x)
    
  } else if (is.call(x)) {
    # Call to function: call linearize recursively
    vars <- lapply(x[-1], linearize, envir = envir)
    
    # Check function call
    if (identical(x[[1]], quote(`(`))) {
      # Call is to a parenthesis: eliminate!
      vars[[1]]
    } else if (identical(x[[1]], quote(`+`)) |
                 identical(x[[1]], quote(`-`)) |
                 identical(x[[1]], quote(`*`))) {
      # Result is either constant or affine
      args <- lapply(vars, `[[`, 'FUN')
      linearize_affine(x[[1]], vars)
      
      ###### CONSTANT ####################
#       if (all(sapply(args, is.numeric))) linearize_constant(out)
      ###### AFFINE ####################
#       else linearize_affine(x[[1]], vars)
      
    } else {
      ###### NONLINEAR ####################
      linearize_nonlinear(x[[1]], vars)
    }
    
  } else if (is.pairlist(x)) {
    # Unsupported for now
    stop("Pairlists are not yet supported. Pass arguments by position instead of by name.")
    
  } else {
    # User supplied some incorrect input
    stop("Can't handle type ", typeof(x), call. = FALSE)
  }
}


# linearize_constant(): linearize a constant expression
# linearize_constant <- function(out) {
#   list(FUN = out)
# }


# linearize_affine(): linearize an affine expression
linearize_affine <- function(f, vars){
  # Update dummies names (if any) so that there is no clash during union
  vars <- Reduce(function(x,y) c(x, list(shift_varnames(y, sum(sapply(x,length)-1)))),
                 vars[-1],
                 init = list(vars[[1]]))
  
  # Form affine call
  out_call <- as.call(c(f, lapply(vars, `[[`, 'FUN')))
  
  # Get and join dummy variables
  dummies <- lapply(vars, function(x) x[setdiff(names(x), 'FUN')])
  dummies <- Reduce(c, dummies)
  
  # Mount expression
  c(dummies, FUN = out_call)
}


# linearize_nonlinear(): linearize a nonlinear expression
linearize_nonlinear <- function(f, vars) {
  # Make nonlinearity atomic, i.e., create a new dummy variable
  # unless the argument is a constant or a single variable
  for (n in seq_along(vars)) {
    if (length(vars[[n]]$FUN) > 1) {
      vars[[n]] <- shift_varnames(vars[[n]], 1)
      vars[[n]] <- c(t1 = substitute(t1 == x, list(x = vars[[n]]$FUN)), vars[[n]])
      vars[[n]]$FUN <- quote(t1)
    }
  }
  
  # Update dummies names so that there is no clash during union
  vars <- Reduce(function(x,y) c(x, list(shift_varnames(y, sum(sapply(x,length)-1)))),
                 vars[-1],
                 init = list(vars[[1]]))
  
  # Form nonlinear atom call
  out_call <- as.call(c(f, lapply(vars, `[[`, 'FUN')))
  
  # Get and join dummy variables
  dummies <- lapply(vars, function(x) x[setdiff(names(x), 'FUN')])
  dummies <- Reduce(c, dummies)
  
  # Create a new epigraph variable for nonlinear atom
  epi_var <- paste0('t', length(dummies)+1)
  
  # Get the curvature of the function
  f_curvature <- get_curvature(match.fun(f))
  
  if (f_curvature == 'convex') {
    # Convex: f <= t
    out_call <- as.call(c(quote(`<=`), out_call, as.name(epi_var)))
    c(dummies, setNames(list(out_call), epi_var), FUN = as.name(epi_var))
    
  } else if (f_curvature == 'concave') {
    # Concave: f >= t
    out_call <- as.call(c(quote(`>=`), out_call, as.name(epi_var)))
    c(dummies, setNames(list(out_call), epi_var), FUN = as.name(epi_var))
    
  } else {
    stop("Can't linearize ", f_curvature, " functions.")
  }
}


# shift_varnames(): 
shift_varnames <- function(expressions, N) {
  n1 <- length(expressions)
  
  if (N != 0 && n1 != 0) {
    var_old <- names(expressions)
    
    # Get dummy variables of the form 'tn'
    dummy_var <- grepl('t\\d', var_old)
    
    # Generate new names: 'tn' -> 't(n+N)'
    var_new <- var_old
    var_new[dummy_var] <- paste0('t', as.numeric(gsub('t', '', var_new[dummy_var])) + N)
    
    # Change the variable names iteratively to avoid clashes
    while (!all(var_new %in% names(expressions))) {
      safe_to_change <- !(var_new %in% names(expressions))
      
      substitutions <- setNames(lapply(as.list(var_new[safe_to_change]), as.name), var_old[safe_to_change])
      expressions <- lapply(expressions, subs_q, env = substitutions)
      names(expressions)[safe_to_change] <- var_new[safe_to_change]
    }
  }
  
  expressions
}



canonicalize_rhs <- function(constraint) {
  rel <- constraint[[1]]
  lhs <- constraint[[2]]
  rhs <- constraint[[3]]
  
  if (identical(rel, quote(`<=`))) {
    # <=
    if (lhs == 0) {
      # Already canonical
      substitute(rhs >= 0, list(rhs = rhs))
      
    } else if (rhs == 0) {
      # Almost canonical, invert everything
      if (!grepl('[+-]', deparse(lhs))) {
        substitute(-lhs >= 0, list(lhs = lhs))
      } else {
        stop('Constraint ', deparse(constraint), 'is too complicated for parser.')
      }
      
    } else {
      # Pull everything to the right and switch inequality
      if (!grepl('[+-]', deparse(lhs))) {
        substitute(rhs - lhs >= 0, list(lhs = lhs, rhs = rhs))
      } else {
        stop('Constraint ', deparse(constraint), 'is too complicated for parser.')
      }
    }
    
  } else if (identical(rel, quote(`>=`))) {
    # >= 
    if (rhs == 0) {
      # Already canonical
      constraint
      
    } else if (lhs == 0) {
      # Almost canonical, invert everything
      if (!grepl('[+-]', deparse(rhs))) {
        substitute(-rhs >= 0, list(rhs = rhs))
      } else {
        stop('Constraint ', deparse(constraint), 'is too complicated for parser.')
      }
      
    } else {
      # Pull everything to the left
      if (!grepl('[+-]', deparse(rhs))) {
        substitute(lhs - rhs >= 0, list(lhs = lhs, rhs = rhs))
      } else {
        stop('Constraint ', deparse(constraint), 'is too complicated for parser.')
      }
    }
    
  } else if (identical(rel, quote(`==`))) {
    # == pull everything to the left side
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
    
  } else {
    # Should never get here!
    stop('Constraint cannot contain ', deparse(rel), '.')
  }
}


add_slack <- function(constraints) {
  nslack <- 0
  
  for (n in seq_along(constraints)){
    rel <- constraints[[n]][[1]]
    lhs <- constraints[[n]][[2]]
    rhs <- constraints[[n]][[3]]
    
    # Sanity check
    if (rhs != 0) {
      stop('Constraint ', deparse(constraint), 'is not canonical.')
    }
    
    if (identical(rel, quote(`>=`))) {
      # New slack variable identifier
      nslack <- nslack + 1
      
      # Add slack variable
      constraints[[n]] <- substitute(lhs + s == 0, list(lhs = lhs, s = as.name(paste0('s',nslack))))
      
    } else if (!identical(rel, quote(`==`))) {
      # Should never get here!
      stop('Constraint ', deparse(constraint), 'is not canonical.')
    }
  }
  
  # Slack nonnegative constraints
  slacks <- list()
  for (n in seq(1, nslack)) {
    slacks <- c(slacks, substitute(s >= 0, list(s = as.name(paste0('s', n)))))
  }
  
  c(constraints, slacks)
}
