#Functions for Generating Synthetic Yield Curve Data

source('NS_Funcs.R')

# Get Betas

getBetas <- function(){
  iniBeta = c(4.5896,-1.9973, -1.1687)
  
  A <- diag(c(0.9979,-0.9777,0.9342))
  
  myCovMat <- 1 / 30 * rWishart(1,3,diag(c(0.1988,0.2920,0.5947)))[,,1]
  
  getNewBeta <- function(currBeta){
    
    A %*% currBeta + MASS::mvrnorm(mu = rep(0,3), Sigma = myCovMat)
    
  }

  n = 100
  betaMat <- matrix(ncol = 3, nrow = n)
  betaMat[1,] <- iniBeta
  currBeta <- iniBeta
  
  for(i in 2:n){
    currBeta <- getNewBeta(currBeta)
    betaMat[i,] <- currBeta
  }
  return(betaMat)
}

# Build Yields

## I will set lambda to be 0.1

getYields <- function(betaMat){
  lam = 0.6915
  tenors <- c(1/12,3/12,6/12,1,2,3,5,7,10,20)
  obsCovMat <- 1 / length(tenors) * rWishart(1,length(tenors),diag(rep(0.05,length(tenors))))[,,1]
  C <- NS(tenors,lam)
  
  getYields <- function(currBeta){
    C %*% currBeta + 
      MASS::mvrnorm(mu = rep(0,length(tenors)), Sigma = obsCovMat)
  }
  
  yieldMat <- matrix(ncol = length(tenors),nrow = n)
  for(i in 1:n){
    yieldMat[i,] <- getYields(betaMat[i,])
  }
  
  return(yieldMat)
}
