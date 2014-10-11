## CVX functions ###################################
cvxfun <- function(...) {
  # Get arguments
  args <- eval(substitute(alist(...)))

  # Make function compatible argument list [e.g., alist(x = , y = )]
  args <- paste0(sapply(args, function(x) {paste0(deparse(x), " = ")}), collapse = ", ")
  args <- eval(parse(text = paste0("alist(", args, ")")))
  
  f <- eval(call("function", as.pairlist(args), quote({})))

  structure(f, class = c("cvxfun", "cvx"))
}


# is.cvxfun(): is a CVX function?
is.cvxfun <- function(x){
  inherits(x, 'cvxfun')
}


# get_curvature(): returns the curvature of a CVX function
# Inherits from class cvx


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


# get_ruleset(): returns the DCP ruleset of a CVX function
get_ruleset <- function(x) {
  if (is.cvxfun(x)){
    attr(x, "ruleset")
  } else {
    stop("DCP rules are only defined for CXV functions.")
  }
}


# print.cvxfun(): formatted printing of the definition of a CVX function
print.cvxfun <- function(x){
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


# +: adds attributes to CVX function prototypes
`+.cvxfun` <- function(e1, e2){
  e2name <- deparse(substitute(e2))
  
  # Add curvature
  if (inherits(e2, 'curvature'))          add_curvature(e1, e2)
  # Add monotonicity
  else if (inherits(e2, 'monotonicity'))  add_monotonicity(e1, e2)
  # Add range
  else if (inherits(e2, 'range'))         add_range(e1, e2)
  # Add DCP rule
  else if (inherits(e2, 'dcprule'))       add_dcprule(e1, e2)
  # Unsupported operation
  else stop("Don't know how to add ", e2name, " to a CVX function", call. = FALSE)
}


# curvature() [TODO]
curvature <- function(x) {
  objname <- deparse(substitute(x))
  
  # If x was already character, strip the ""
  objname <- gsub('"', '', objname)
  
  structure(objname, class = c('curvature', 'cvx'))
}


# monotonicity() [TODO]
monotonicity <- function(x){
  objname <- deparse(substitute(x))
  
  # If x was already character, strip the ""
  objname <- gsub('"', '', objname)
  
  structure(objname, class = c('monotonicity', 'cvx'))
}


# Add stuff to CVX functions
add_curvature <- function(e1,e2)      { attr(e1, "curvature") <- e2; e1 }
add_monotonicity <- function(e1,e2)   { attr(e1, "monotonicity") <- e2; e1 }
add_range <- function(e1,e2)          { attr(e1, "range") <- e2; e1 }

add_dcprule <- function(e1, e2){
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
dcprule <- function(e1cond, e2cond, result){
  structure(c(substitute(e1cond), substitute(e2cond), substitute(result)),
            class = c("dcprule", "cvx"))
}


# print.dcprule(): shows the definition of a DCP rule
print.dcprule <- function(x){
  cat("DCP rule")
  cat("\n")
  cat("Argument:", deparse(x[[1]]))
  cat("\n")
  cat("Argument:", deparse(x[[2]]))
  cat("\n")
  cat("Results:", deparse(x[[3]]))
  cat("\n")
}


# ==.dcprule: compare DCP rules
`==.dcprule` <- function(r1, r2){
  if(length(r1) != length(r2)){
    stop("Rules must have the same length to be compared.")
  }
  
  for (n in seq_along(r1)){
    if(r1[[n]] != r2[[n]]) return(FALSE)
  }
  
  TRUE
}


# dcp_check_rule(): checks if rule applies to arguments e1 and e2
# Returns either the curvature of the result or FALSE
dcp_check_rule <- function(rule, e1, e2){
  substitutions <- list(convex   = quote(get_curvature(x) == "convex"),
                        concave  = quote(get_curvature(x) == "concave"),
                        affine   = quote(get_curvature(x) == "affine"),
                        constant = quote(get_curvature(x) == "constant"))
  
  rule[-3] <- lapply(rule[-3], subs_q, env = substitutions)
  
  
  if (eval(dcp_build_test(rule[[1]], e1), enclos = emptyenv()) &&
        eval(dcp_build_test(rule[[2]], e2), enclos = emptyenv())) {
    deparse(rule[[3]])
  } else {
    FALSE
  }
}


# dcp_build_test: builds an expression to check condition cond on argument e1
dcp_build_test <- function(cond, x) subs_q(cond, list(x = substitute(x)))


dcpcheck <- function(FUN, e1, e2){
  fname <- deparse(substitute(FUN))
  ruleset <- get_ruleset(FUN)
  
  if (is.null(ruleset)){
    # Function has no DCP ruleset => its result is undefined
    stop("Function {", fname, "} has no ruleset.", call. = FALSE)
  }
  
  # Check DCP rules
  test_result <- lapply(ruleset, function(x) dcp_check_rule(x, e1, e2))
  test_result <- unique(test_result)
  test_result <- test_result[sapply(test_result, is.character)]
  
  if (length(test_result) > 1){
    # If there are more than 2 unique solutions, then the ruleset is inconsistent
    # (i.e., arguments evaluate to different curvatures)
    stop('The DCP ruleset of ', fname, ' is inconsistent: results evaluated to ', test_result)
  }
  
  if (length(test_result) == 0){
    stop(paste0("You are not allowed to apply {", fname, "} on ",
                "{", get_curvature(e1), "}", " and ",
                "{", get_curvature(e2), "}"), call. = FALSE)
  }
  
  test_result[[1]]
}
