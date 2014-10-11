## Common CVX methods ############################
# is.scalar(): check if argument is scalar (1 x 1)
is.scalar <- function(x){
  if(!is.numeric(x) && !is.cvx(x)){
    stop('A numeric value or CVX object is required.')
  }
  
  if(is.null(dim(x)) || all(dim(x) == c(1,1))){
    TRUE
  } else{
    FALSE
  }
}


# subs_q(): substitute that works with quoted expressions
subs_q <- function(x, env){
  call <- substitute(substitute(y, env), list(y = x))
  eval(call)
}
