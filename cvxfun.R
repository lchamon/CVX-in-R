## CVX functions ###################################
cvxfun <- function(...) {
  # Get arguments
  args <- eval(substitute(alist(...)))
  args_name <- sapply(args, deparse)

  # Make function compatible argument list [e.g., alist(x = , y = )]
  args <- paste0(sapply(args, function(x) {paste0(deparse(x), " = ")}), collapse = ", ")
  args <- eval(parse(text = paste0("alist(", args, ")")))
  
  cvxfun_body <- paste0(
    '{', '\n',
    paste0('force(', args_name, ')', collapse = '\n'),
    '\n',
    'fcall <- match.call()[[1]]', '\n',
    'FUN <- match.fun(fcall, descend = FALSE)', '\n',
    'fname <- deparse(fcall)', '\n',
    'curv <- dcpcheck(fname = fname, FUN = FUN,', paste0(args_name, collapse = ', '), ')', '\n',
    'cvx(curvature = curv)', '\n',
    '}'
  )
  
  f <- eval(call("function", as.pairlist(args), quote({})))
  body(f) <- parse(text = cvxfun_body)

  # Function body example
  # {
  # force(x)
  # force(y)
  # fcall <- match.call()[[1]]
  # FUN <- match.fun(fcall, descend = FALSE)
  # fname <- deparse(fcall)
  # curv <- dcpcheck(fname = fname, FUN = FUN, x, y)
  # cvx(curvature = curv)
  # }

  structure(f, class = c("cvxfun"))
}


# is.cvxfun(): is a CVX function?
is.cvxfun <- function(x){
  inherits(x, 'cvxfun')
}


# get_curvature(): returns the curvature of a CVX function
# Inherits from class cvx
get_curvature.cvxfun <- get_curvature.cvx


# get_monotonicity(): returns the monotonicity of a CVX function
get_monotonicity <- function(x) {
  if (is.cvxfun(x)) {
    attr(x, "monotonic")
  } else {
    stop("Monotonicity is only defined for CXV functions.")
  }
}


# get_range(): returns the range of a CVX function
# Inherits from class cvx
get_range.cvxfun <- get_range.cvx


# get_ruleset(): returns the DCP ruleset of a CVX function
get_ruleset <- function(x) {
  if (is.cvxfun(x)){
    attr(x, "ruleset")
  } else {
    stop("DCP rules are only defined for CXV functions.")
  }
}


# get_epigraph(): returns the epigraph implementation a CVX function
get_epigraph <- function(x) {
  if (is.cvxfun(x)){
    if (!is.null(attr(x, "epigraph"))){
      attr(x, "epigraph")
    } else {
      stop("The CXV functions has no epigraph implementation.", call. = FALSE)
    }
  } else {
    stop("Only CXV functions have epigraph implementation.")
  }
}


# print.cvxfun(): formatted printing of the definition of a CVX function
print.cvxfun <- function(x, ...){
  cat("CVX function")
  cat("\n")
  cat("Arguments:",
      paste(names(formals(x)), collapse = ", "))
  cat("\n")
  
  if(!is.null(get_curvature(x))){
    cat("Curvature:", get_curvature(x))
    cat("\n")
  }
  
  if(!is.null(get_monotonicity(x))){
    cat("Monotonicity:", get_monotonicity(x))
    cat("\n")
  }
  
  cat("Range:", get_range(x))
  cat("\n")

  if(!is.null(get_ruleset(x))){
    cat("\n")
    cat("DCP rule set")
    cat("\n")
    
    for (rule in get_ruleset(x)){
      print(rule)
      cat("\n")
    }
  }
}


# +.cvxfun: adds attributes to CVX function prototypes
`+.cvxfun` <- function(e1, e2){
  e2name <- deparse(substitute(e2))
  
  # Add curvature
  if (inherits(e2, 'cvx_curvature'))         add_curvature(e1, e2)
  # Add monotonicity
  else if (inherits(e2, 'cvx_monotonicity')) add_monotonicity(e1, e2)
  # Add range
  else if (inherits(e2, 'range'))            add_range(e1, e2)
  # Add epigraph implementation
  else if (inherits(e2, 'epigraph'))         add_epigraph(e1, e2)
  # Add DCP rule
  else if (inherits(e2, 'dcprule'))          add_dcprule(e1, e2)
  # Unsupported operation
  else stop("Don't know how to add ", e2name, " to a CVX function.", call. = FALSE)
}


# [TODO] curvature()
curvature <- function(x) {
  objname <- deparse(substitute(x))
  
  # If x was already character, strip the ""
  objname <- gsub('"', '', objname)
  
  structure(objname, class = 'cvx_curvature')
}


# [TODO] monotonicity()
monotonicity <- function(x){
  objname <- deparse(substitute(x))
  
  # If x was already character, strip the ""
  objname <- gsub("\"", "", objname)
  
  structure(objname, class = 'cvx_monotonicity')
}

# [TODO] epigraph()
epigraph <- function(x){
  epi <- substitute(x)
  structure(epi, class = 'epigraph')
}


# Add stuff to CVX functions
add_range <- function(e1,e2)          { attr(e1, "range") <- unclass(e2); e1 }

# [TODO]
# As soon as a curvature is added, the DCP rules related to
# affine inputs should also be added. Then, the monotonicity
# needs to be checked to add rules relating to other input
# curvatures.
add_curvature <- function(e1,e2)      { attr(e1, "curvature") <- unclass(e2); e1 }

# [TODO]
# As soon as a monotonicity is added, the curvature of the
# function needs to be checked to add rules relating to
# arbitrary input curvatures.
add_monotonicity <- function(e1,e2)   { attr(e1, "monotonicity") <- unclass(e2); e1 }

add_epigraph <- function(e1, e2) {
  r1 <- cvx()
  prob <- eval(e2)
  
  if (!is.cvxprob(prob)) {
    stop("Epigraph implementations must be CVX problems.")
  }
  
  attr(e1, "epigraph") <- prob
  e1
}

add_dcprule <- function(e1, e2) {
  nargs_e1 <- length(formals(e1))
  nargs_e2 <- length(e2) - 1
  
  if (nargs_e1 != nargs_e2) {
    stop("The number of arguments in the function does not match the DCP rule.")
  }
  
  ruleset <- get_ruleset(e1)
  
  if (is.null(ruleset)) {
    attr(e1, "ruleset") <- list(e2)
  } else {
    attr(e1, "ruleset") <- c(ruleset, list(e2))
  }
  
  e1
}


## DCP rules methods ###################################
# dcprule(): create new DCP rule
dcprule <- function(..., out){
  conds <- eval(substitute(alist(...)))
  
  if(missing(out)){
    stop('You must provide an outcome for the DCP rule.')
  }
  
  structure(c(conds, substitute(out)),
            class = c("dcprule"))
}


# is.dcprule(): is a DCP rule?
is.dcprule <- function(r){
  inherits(r, 'dcprule')
}


# print.dcprule(): shows the definition of a DCP rule
print.dcprule <- function(r){
  rlen <- length(r)
  
  cat("DCP rule")
  cat("\n")
  
  if (rlen > 1){
    for (n in seq(1, rlen-1)){
      cat("Argument:", deparse(r[[n]]))
      cat("\n")
    }
  }

  cat("Results:", deparse(r[[rlen]]))
  cat("\n")
}


# ==.dcprule: compare DCP rules
`==.dcprule` <- function(r1, r2){
  stopifnot(is.dcprule(r1), is.dcprule(r2))
  
  if(length(r1) != length(r2)){
    stop("Rules must have the same length to be compared.")
  }
  
  identical(r1, r2)
}


# dcp_check_rule(): checks if rule applies to arguments e1 and e2
# Returns either the curvature of the result or FALSE
dcp_check_rule <- function(rule, args){
  if (!is.dcprule(rule)) {
    stop("You must provide a DCP rule for checking.")
  }

  rule_nargs <- length(rule) - 1
  rule_out <- length(rule)

  if (length(args) != rule_nargs) {
    stop("The number of arguments provided does not match the rule.")
  }
  
  substitutions <- list(convex   = quote(get_curvature(x) == "convex"),
                        concave  = quote(get_curvature(x) == "concave"),
                        affine   = quote(get_curvature(x) == "affine"),
                        constant = quote(get_curvature(x) == "constant"))
  
  rule[-rule_out] <- lapply(rule[-rule_out], subs_q, env = substitutions)
  
  check <- mapply(function(r, arg) {
                    out <- eval(dcp_build_test(r, arg), enclos = emptyenv())
                    if (identical(out, logical(0))) FALSE else out
                  },
                  rule[-rule_out],
                  args)
  
  if (all(unlist(check))) {
    deparse(rule[[rule_out]])
  } else {
    FALSE
  }
}


# dcp_build_test: builds an expression to check condition [cond] on argument [x]
dcp_build_test <- function(cond, x) subs_q(cond, list(x = substitute(x)))


# dcpcheck: checks DCP ruleset of a CVX function
dcpcheck <- function(FUN, fname, ...){
  if (missing(fname)) {
    fname <- deparse(substitute(FUN))
  }
  
  if (!is.cvxfun(FUN)) {
    stop(fname, " is not a CVX functions.", call. = FALSE)
  }

  ruleset <- get_ruleset(FUN)
  
  if (is.null(ruleset)){
    # Function has no DCP ruleset => its result is undefined
    stop("Function [", fname, "] has no ruleset.", call. = FALSE)
  }
  
  # Check DCP rules
  test_result <- lapply(ruleset, function(r) dcp_check_rule(r, list(...)))
  test_result <- unique(test_result)
  test_result <- test_result[sapply(test_result, is.character)]
  
  if (length(test_result) > 1){
    # If there are more than 2 unique solutions, then the ruleset is inconsistent
    # (i.e., arguments evaluate to different curvatures)
    stop('The DCP ruleset of ', fname, ' is inconsistent: results evaluated to ', test_result)
  } else if (length(test_result) == 0){
    # If test results are empty, then all rules were violated
    stop("DCP violation: you are not allowed to do ", fname, "(",
         paste0("{",
                sapply(list(...), get_curvature),
                "}",
                collapse = ', '),
         ").",
         call. = FALSE)
  } else {
    # Return the curvature evaluated by the DCP ruleset
    test_result[[1]]
  }
}
