gbu <- read.csv('Clean_Yields.csv')
dates <- gbu[,1]
gbu <- gbu[,-1]
row.names(gbu)<- dates
names(gbu) <- c("1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","20Y","30Y")
gbu <- gbu[rowSums(is.na(gbu)) != ncol(gbu),]


tenors <- c(1 / 12, 3 / 12, 6 / 12, 1, 2, 3, 5, 7, 10, 20, 30)

NS <- function(tau,lam){
  
  return( matrix(c(rep(1,times = length(tau)),sapply(tau,function(x) (1-exp(-lam * x)) / (lam * x)),
                   sapply(tau,function(x) (1-exp(-lam * x)) / (lam * x) - exp(-lam * x))),ncol = 3))
  
}

# beta <- function(y,tau,lam){
#   phi <- NS(tau,lam)
#   return(solve((t(phi) %*% phi)) %*% t(phi) %*% y)
# }

covMatEst <- function(y,tau,lam){
  phi <- NS(tau,lam)
  
  projMat <- phi %*% solve((t(phi) %*% phi)) %*% t(phi)
  
  e <- (diag(nrow(phi)) - projMat) %*% y
  
  return(e %*% t(e))
}

myMat <- function(n){
  iniMat <- matrix(0,nrow = 11,ncol = 11)
  for(i in (nrow(gbu) - n):nrow(gbu)){
    iniMat <- iniMat + covMatEst(t(gbu[i,]),tenors,0.3)
  }
  
  return(iniMat / (n - 3))
}

MannyResult <- myMat(5000)