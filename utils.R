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
