source("NS_Funcs.R")
source("aveBondSrc/bootstrapFuncs.R")

library(MTS)
library(MASS)
library(expm)
library(plyr)
# library(MTS)


.yieldMC <- function(yields) { 
  # valDate: Date of valuation of the portfolio
  # tenors: tenors used for the simulation
  # yields: information on which the model is trained
  
  # change them to a list where everything is compatible

  lambda <- 0.33
  
  # retrieve the tenors as strings
  tenorChar <- colnames(yields)
  dates <- rownames(yields)
  
  for(i in 1:length(colnames(yields))) {
    tenorNum <- c()
    if (!is.na(yields[i,]["1M"])) {
      tenorNum <- c(tenorNum, 1/12)
    }
    if (!is.na(yields[i,]["3M"])) {
      tenorNum <- c(tenorNum, 3/12)
    }
    if (!is.na(yields[i,]["6M"])) {
      tenorNum <- c(tenorNum, 6/12)
    }
    if (!is.na(yields[i,]["1Y"])) {
      tenorNum <- c(tenorNum, 1)
    }
    if (!is.na(yields[i,]["2Y"])) {
      tenorNum <- c(tenorNum, 2)
    }
    if (!is.na(yields[i,]["3Y"])) {
      tenorNum <- c(tenorNum, 3)
    }
    if (!is.na(yields[i,]["5Y"])) {
      tenorNum <- c(tenorNum, 5)
    }
    if (!is.na(yields[i,]["7Y"])) {
      tenorNum <- c(tenorNum, 7)
    }
    if (!is.na(yields[i,]["10Y"])) {
      tenorNum <- c(tenorNum, 10)
    }
    if (!is.na(yields[i,]["20Y"])) {
      tenorNum <- c(tenorNum, 20)
    }
  }
  
  
  
  # cubic spline interp.
  myTens <- seq(1, max(tenorNum),length.out = 20)

  # fit static nelson siegel function
  fit_nelson_siegel <- function(yields, lambda, tenors,newTen, tol = 0.01){
    # yield_list: Parameter of the form of a list of data frames containing ZCB spot rate
    # lambda: Individual lambda parameter for NS
    # start: starting date from the yield_list list
    # tenors: list of time to maturities
    # T_: length of time window
    
    # yieldsInterp <- interp_yc(tenorNum, yields, myTens))
    

    
    df <- as.data.frame(yields)
    x <- as.data.frame(unique(is.na(df[!complete.cases(df),])))
    
    # Iterate through rows of x
    
    if(length(x) == 0){
      yieldMat <- matrix(ncol = ncol(yields), nrow = 0)
      for(i in 1:nrow(x)){
  
        currMat <- df[which(rownames(df) %in% rownames(match_df(
          as.data.frame(is.na(df[!complete.cases(df),])), x[i,]))),]
  
        currRowNames <- rownames(currMat)
        golInd <- which(colSums(!is.na(currMat)) != 0)
        currMat <- as.matrix(currMat[,golInd])
        
        
        currMat <- interp_yc(tenors[golInd],currMat,newTen = newTen)
        rownames(currMat) <- currRowNames
        
        yieldMat <- rbind(yieldMat,currMat)
      }
      
      yieldMat <- yieldMat[order(rownames(yieldMat)),]
      
    }else{
      yieldMat <- interp_yc(tenors, yields, newTen = newTen)
    }
    
    Phi <- NS(newTen,lambda) # Construct Phi matrix for NS
    prevBetas <- solve(t(Phi) %*% Phi) %*% t(Phi) %*% t(yieldMat)
    currTol = 1
    
    while(currTol > tol){
      eps <- yieldMat - t(Phi %*% prevBetas)
      sig_hat2 <- t(eps) %*% eps / (nrow(yieldMat) - 3)
      
      if(rcond(sig_hat2) < 0.0001){
        sig_hat2 <- sig_hat2 + diag(nrow(sig_hat2)) * 0.0001 * max(diag(sig_hat2))
      }
      
      L1 <- solve(t(chol(sig_hat2)))
      Phi_ <- L1 %*% Phi
      yMat_ <- L1 %*% t(yieldMat)
      currBetas <- solve(t(Phi_) %*% Phi_) %*% t(Phi_) %*% yMat_
      currTol = norm(currBetas - prevBetas) / norm(prevBetas)
      
      prevBetas <- currBetas
      
    }
    betas = currBetas
    
    return(list(betas = betas, # fitted betas static: 1*3   dynamic: T*3
                sigma2 = sig_hat2, yieldMat = yieldMat)) # lambda(input)
    # cov_mat_betas = sig_hat2 * solve(t(Phi) %*% Phi), # Nelson Siegel design matrix N*3
    # eps = eps,
    # Phi = Phi)) # residuals N*T
  }
  
  # define YW AR(1) function
  yule_walker_ar1 <- function(ts) {
    # Estimates the parameters of an AR(1) process using the Yule-Walker equations.
    #
    # Parameters:
    # ts: A univariate time series (numeric vector).
    #
    # Returns:
    # A list containing:
    #   phi: The estimated autoregressive coefficient.
    #   sigma2: The estimated variance of the innovations.
    #   c: The estimated intercept term.
    
    # Compute the sample mean (used for centering the time series)
    mu <- mean(ts)
    
    # Center the time series
    ts_centered <- ts - mu
    
    # Compute the autocovariance at lag 0 and lag 1
    gamma0 <- var(ts_centered) * (length(ts_centered) - 1) / length(ts_centered)
    gamma1 <- sum(ts_centered[-1] * ts_centered[-length(ts_centered)]) / length(ts_centered)
    
    # Estimate phi using the Yule-Walker equation
    phi <- gamma1 / gamma0
    
    # Estimate the intercept c
    c <- mu * (1 - phi)
    
    # Estimate the innovation variance sigma2
    sigma2 <- gamma0 * (1 - phi^2)
    
    # Return the estimates
    return(list(phi = phi, sigma2 = sigma2, c = c))
  }
  
  
  funkyOut <- fit_nelson_siegel(yields, 0.33, tenorNum, myTens)
  
  tsBetas <- funkyOut$betas
  
  # parameters 
  # A <- diag(3)
  # diag(A) <- c(yule_walker_ar1(tsBetas[1,])$phi,
  #              yule_walker_ar1(tsBetas[2,])$phi,
  #              yule_walker_ar1(tsBetas[3,])$phi)
  capture.output(myVarma <- VARMA(t(tsBetas), p = 1))
  A <- myVarma$Phi
  Q <- myVarma$Sigma
  
  # diag(A) <- c(0.95,0.9,0.8)
  
  # Q <- diag(3)
  
  # diag(Q) <- c(yule_walker_ar1(tsBetas[1,])$sigma2,
  #              yule_walker_ar1(tsBetas[2,])$sigma2,
  #              yule_walker_ar1(tsBetas[3,])$sigma2)
  
  
  # R <- diag(length(tenorNum)) * fit_nelson_siegel(yieldsInterp, 
  #                                                 0.33, 1, 
  #                                                 nsTenors, 1)$sigma2
  
  R <- funkyOut$sigma2
  yieldMat <- funkyOut$yieldMat
  C <- NS(myTens,lambda)
  
  # icpt <- (diag(3) - A) %*% rowMeans(tsBetas)
  icpt <- myVarma$Ph0
  
  KF_loop <- function(A, B, C, D, R, Q, icpt, yields,beta_values) {
    N <- ncol(yields)
    # Preallocate list for observations (assumes yields is a vector or similar)
    T_ <- nrow(yields)
    y_t <- vector("list", T_)
    
    for (t in 1:T_) {
      y_t[[t]] <- as.matrix(yields[t,])  
    }

    # Initial state (x_{0|0}) and covariance (Sigma_{0|0})
    last_x <- matrix(c(5, -3, 3), nrow = 3, ncol = 1) # initialize at OLS
    last_Sig <- diag(3)
    
    # Preallocate matrices to store expected observations and actual observations.
    # Here, we assume the observation dimension is determined by the rows of C.
    n_y <- nrow(C)
    E_y_vec <- matrix(0, nrow = N, ncol = T_)
    y_real <- matrix(0, nrow = N, ncol = T_)
    E_x_vec <- matrix(0, nrow = 3, ncol = T_)
    x_real <- matrix(0, nrow = 3, ncol = T_)
    x_var_pred <- list()
    x_var_real <- list()
    xStates <- matrix(0, nrow = 3, ncol = T_)
    
    
    lower_bound <- matrix(0, nrow = N, ncol = T_)
    upper_bound <- matrix(0, nrow = N, ncol = T_)
    
    # Kalman Filter loop over time
    for (i in 1:T_) {
      # One-step ahead prediction for state and covariance
      E_x_t <- A %*% last_x + icpt
      Var_x_t <- A %*% last_Sig %*% t(A) + B %*% Q %*% t(B)
      
      # One-step ahead prediction for observation and its covariance
      E_y_t <- C %*% E_x_t
      F_t <- C %*% Var_x_t %*% t(C) + D %*% R %*% t(D)
      # Compute Kalman Gain
      
      # Compute Kalman Gain
      K_t <- Var_x_t %*% t(C) %*% solve(F_t)
      

      
      # Innovation: difference between actual and predicted observation
      e_t <- y_t[[i]] - E_y_t
      
      # Update state estimate and covariance with innovation
      next_x <- E_x_t + K_t %*% e_t
      next_Sig <- Var_x_t - K_t %*% C %*% Var_x_t
      
      # Store the predicted observation and the actual observation
      E_y_vec[, i] <- as.vector(E_y_t)
      y_real[, i] <- as.vector(y_t[[i]])
      E_x_vec[, i] <- as.vector(E_x_t)
      xStates[, i] <- as.vector(next_x)
      std_dev <- sqrt(diag(F_t))  
      lower_bound[, i] <- E_y_vec[, i] - 1.96 * std_dev
      upper_bound[, i] <- E_y_vec[, i] + 1.96 * std_dev
      x_var_pred[[i]] <- Var_x_t
      x_var_real[[i]] <- next_Sig
      #x_real[, i] <- as.vector(y_t[[i]])
      
      
      # Update variables for next iteration
      last_x <- next_x
      last_Sig <- next_Sig
    }
    
    
    return(list(xStates = xStates , x_var_real = x_var_real,obsCov = D %*% R %*% t(D),
                baseCov = B %*% Q %*% t(B)))
  } 
  
  
  kfRun <- KF_loop(A, diag(3), C, diag(ncol(yieldMat)), R, Q, icpt, yieldMat, tsBetas)
  
  states <- kfRun$xStates
  covs <- kfRun$x_var_real
  
  obsCov <- kfRun$obsCov
  baseCov <- kfRun$baseCov
  
  lastIdx <- ncol(states)
  
  lastState <- states[,lastIdx]
  lastCov <- covs[[lastIdx]]
  lastDate <- dates[lastIdx]

  outPut <- list(A,lastState,lastCov,obsCov,baseCov,lastDate,C,myTens,icpt)
  
  return(outPut)
}


# yieldMat <- matrix(0, nrow = 50, ncol = 10)
# yieldMock <- c(1.5, 2.1, 2.4, 
#                 2.6, 3, 3.1, 
#                3.15, 3.2, 3.23, 3.23)
# for(i in 1:50) {
#   yieldMat[i,] <- yieldMock + mvrnorm(mu = rep(0, 10), Sigma = 0.01 * diag(10))
# }
# chartenors <- c('1M', '3M', '6M', '1Y',
#             '2Y', '3Y', '5Y', '7Y',
#             '10Y', '20Y')
# colnames(yieldMat) <- chartenors
# 
# dates <- seq(as.Date("1900-01-01"), by = "day", length.out= 50)
# 
# 
# rownames(yieldMat) <- as.character(dates)
# 
# 



