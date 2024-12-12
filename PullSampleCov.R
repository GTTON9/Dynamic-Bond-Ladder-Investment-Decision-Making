gbu <- read.csv('Clean_Yields.csv')
dates <- gbu[,1]
gbu <- gbu[,-1]
row.names(gbu)<- dates
names(gbu) <- c("1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","20Y","30Y")
gbu <- na.omit(gbu[rowSums(is.na(gbu)) != ncol(gbu),])

source('B-Spline Functions.R')

tenors <- c(1 / 12, 3 / 12, 6 / 12, 1, 2, 3, 5, 7, 10, 20, 30)

### Examination
newTen <- c(1/12,3 / 12,6 / 12,seq(from = 1,to = 20,1))

myKnots <- quantile(tenors,c(0,0.25,0.5,0.75,1))
regX <- bSpline(newTen,myKnots)
for(i in 1:nrow(gbu)){
  y <- t(gbu[i,])
  B <- bSpline(tenors,myKnots)
  
  if(i == 1){
    yPred <- as.vector(regX %*% solve(t(B) %*% B) %*% t(B) %*% y)
  }else{
    yPred <- rbind(yPred,as.vector(regX %*% solve(t(B) %*% B) %*% t(B) %*% y))
  }
}
colnames(yPred) <- newTen

### NS Model

NS <- function(tau,lam){
  
  return( matrix(c(rep(1,times = length(tau)),sapply(tau,function(x) (1-exp(-lam * x)) / (lam * x)),
                   sapply(tau,function(x) (1-exp(-lam * x)) / (lam * x) - exp(-lam * x))),ncol = 3))
  
}

beta <- function(y,tau,lam){
  phi <- NS(tau,lam)
  return(solve((t(phi) %*% phi)) %*% t(phi) %*% y)
}



matFunc <- function(y,tau,lam){
  phi <- NS(tau,lam)
  
  projMat <- phi %*% solve((t(phi) %*% phi)) %*% t(phi)
  
  e <- (diag(nrow(phi)) - projMat) %*% y
  
  return(e %*% t(e))
}




myMat <- function(n){
  iniMat <- matrix(0,nrow = length(newTen),ncol = length(newTen))
  for(i in (nrow(gbu) - n):nrow(yPred)){
    iniMat <- iniMat + matFunc(yPred[i,],newTen,0.1)
  }
  
  return(iniMat / (n - 3))
}

MannyResult <- myMat(500)
