# Here are all of the Nelson Siegel functions

NS <- function(tau,lam){
  
  return( matrix(c(rep(1,times = length(tau)),sapply(tau,function(x) (1-exp(-lam * x)) / (lam * x)),
                   sapply(tau,function(x) (1-exp(-lam * x)) / (lam * x) - exp(-lam * x))),ncol = 3))
  
}

beta <- function(y,tau,lam){
  phi <- NS(tau,lam)
  return(solve((t(phi) %*% phi)) %*% t(phi) %*% y)
}