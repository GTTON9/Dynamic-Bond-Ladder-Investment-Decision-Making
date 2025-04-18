library(pracma)

#Nelson Siegel Functions
NS <- function(tau,lam){
  
  return( matrix(c(rep(1,times = length(tau)),sapply(tau,function(x) (1-exp(-lam * x)) / (lam * x)),
                   sapply(tau,function(x) (1-exp(-lam * x)) / (lam * x) - exp(-lam * x))),ncol = 3))
  
}

beta <- function(y,tau,lam){
  phi <- NS(tau,lam)
  return(solve((t(phi) %*% phi)) %*% t(phi) %*% y)
}

covMatEst <- function(y,phi,betas){

  e <- y - phi %*% betas
  
  return(e %*% t(e))
}

# Data Generators

generate_data <- function(T_, betas, covMat, lambda, tenors) {
  # T_: length of time window
  # betas: state vector (L, S, C)
  # covMat: covariance matrix over tenors
  # lambda: NS tuning parameter
  
  L <- t(chol(covMat))
  n <- length(tenors)
  
  
  X <- NS(tenors,lambda)
  
  for(i in 1:T_){
    eps <-  L %*% rnorm(n)
    if(i == 1){
      currDF <- as.vector(X %*% betas + eps)
    }
    else{
      currDF <- rbind(currDF,as.vector(X %*% betas + eps))
    }
  }
  colnames(currDF) <- tenors
  return(currDF)
}

windowGen <- function(period, periodCount, betaMat, covMat, lambda, tenors){
  
  currBetaIndex <- 1
  for(i in 1:(periodCount)){
    
    betas <- betaMat[i,]
    
    addData <- generate_data(period, betas, covMat, lambda, tenors)
    
    
    if(i == 1){
      currData <- addData
    }
    else{
      currData <- rbind(currData,addData)
    }
      
  }
  return(currData)
}
  

  
# Tests
  
# GLS Function

# For known and fixed lambda.

recurGLS <- function(datMat,lambda,tol = 0.05){
  tenors <- as.numeric(colnames(datMat))
  n <- length(tenors)
  T_ <- nrow(datMat)
  currTol <- 1
  
  init <- F
  X <- NS(tenors,lambda)
  
  currBetas <- rowMeans(solve(t(X) %*% X) %*% t(X) %*% t(datMat))
  
  while((!init) | (currTol > tol)){
    
    e <- t(datMat) - as.vector(X %*% currBetas)
    currMat <- e %*% t(e) / (T_ - 3)
    

    if(!init){
      init = T
      prevCov <- currMat
    }
    else{
      currTol <- norm(currMat - prevCov) / norm(prevCov)
    }
    
    # om <- pinv(currMat)
    
    L_m <- solve(t(chol(currMat)))

    X_tilde <- L_m %*% X
    Y_tilde <- L_m %*% t(datMat)
    currBetas <- rowMeans(solve(t(X_tilde) %*% X_tilde) %*% t(X_tilde) %*% Y_tilde)
    
    # currBetas <- rowMeans(solve(t(X) %*% om %*% X) %*% t(X) %*% om %*% t(datMat))
    print(currTol)
  }
  
  return(currBetas)
}

finalCov <- (rWishart(1,20,diag(diag(MannyResult))))[,,1] / 20
newDat <- generate_data(100,betas = c(4, -5, 4), covMat = finalCov,lambda = 0.1,
                        tenors = seq(1,20,1))

betaMat <- matrix(c(4, 0, -5,
                    3,2,-3,
                    5,-7,8),nrow = 3,byrow = T)

moreDat <- windowGen(100,3,betaMat,myCovMat,0.1,tenors = seq(2,20,2))

currBetas <- recurGLS(moreDat,0.1,tol = 0.01)



