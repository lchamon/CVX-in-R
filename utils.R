## Common CVX methods ############################
# is.scalar(): check if argument is scalar (1 x 1)
is.scalar <- function(x){
  if(!is.numeric(x) && !is.cvx(x)){
    stop('A numeric value or CVX object is required.')
  }
  
  # Fix the null case assuming a column vector
  if (is.null(dim(x))){
    dim(x) <- c(length(as.list(x)),1)
  }
  
  if(all(dim(x) == c(1,1))){
    TRUE
  } else{
    FALSE
  }
}


# is.whole(): is a whole number?
is.whole <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}


# subs_q(): substitute that works with quoted expressions
subs_q <- function(x, env){
  call <- substitute(substitute(y, env), list(y = x))
  eval(call)
}


# shift_vars(): takes a list of expressions and shifts all variables
# of the form pattern by N. If reset is TRUE, then it also restarts the
# variable numbering from 1.
shift_vars <- function(expressions, N = 0, pattern = 't\\d', reset = TRUE) {
  # Get dummy variables of the form [pattern]
  var_old <- get_vars(expressions, pattern = pattern)
  
  if (length(var_old) != 0) {
    # Generate new names: 't(n+N)' (n = 1,2,...)
    if (reset) {
      var_new <- paste0(gsub('\\d', '', pattern, fixed = TRUE),
                        seq(1,length(var_old)) + N)
    } else {
      prefix <- gsub('\\d', '', pattern, fixed = TRUE)
      var_new <- paste0(prefix,
                        as.numeric(gsub(prefix, '', var_old)) + N)
    }
    
    # Change the variable names iteratively to avoid clashes
    dummy_var <- get_vars(expressions, pattern = pattern)
    
    while (!all(var_new %in% dummy_var)) {
      safe_to_change <- !(var_new %in% dummy_var)
      
      substitutions <- setNames(lapply(as.list(var_new[safe_to_change]), as.name), var_old[safe_to_change])
      expressions <- lapply(expressions, subs_q, env = substitutions)
      
      dummy_var <- get_vars(expressions, pattern = pattern)
    }
  }
  
  expressions
}


# get_vars(): generic method to get variable names from expressions
get_vars <- function(x, pattern = '') UseMethod('get_vars')

get_vars.default <- function(x, pattern = '') {
  stop("There is no method to get variables names of object ", typeof(x))
}

get_vars.numeric <- function(x, pattern = '') {
  character()
}

get_vars.name <- function(x, pattern = '') {
  if (grepl(pattern, as.character(x))) {
    as.character(x)
  } else {
    character()
  }
}

get_vars.call <- function(x, pattern = '') {
  if (is.atomic(x)) {
    character()
    
  } else if (is.name(x)) {
    as.character(x)
    
  } else if (is.call(x)) {
    h <- unlist(lapply(x[-1], get_vars))
    h[grepl(pattern, h)]
    
  } else if (is.pairlist(x)) {
    stop("Named arguments are unsupported: ", deparse(x))
    
  } else {
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

get_vars.list <- function(x, pattern = '') {
  unique(unlist(lapply(x, get_vars, pattern = pattern)))
}
