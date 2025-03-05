source("C:/Users/ander/Documents/PSTAT 296/PSTAT296-Project/NS_Funcs.R")
library(MASS)
library(expm)


.yieldMC <- function(valDate, tenors, yields) { 
  # valDate: Date of valuation of the portfolio
  # tenors: tenors used for the simulation
  # yields: information on which the model is trained
  
  # change them to a list where everything is compatible
  yieldList <- list()
  
  # retrieve the tenors as strings
  tenorChar <- colnames(yields)
   
  dates <- rownames(yields)

  for(i in 1:nrow(yields)) {
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
    dfYield <- data.frame(Maturity = tenorNum,
                          ZERO_YLD1 = yields[i,])
    yieldList[[i]] <- dfYield
  }
  
  # define the cox de boor basis polynomials
  basis <- function(x, degree, i, knots){
    if(degree == 0){
      B <- ifelse((x>=knots[i])&(x<knots[i+1]),1,0)
    } else {
      if((knots[degree + i] - knots[i]) == 0){
        if(x != knots[i+degree]){
          alpha1 <- 0
        } else {
          return(1)
        }
      } else {
        alpha1 <- (x-knots[i])/(knots[degree+i] - knots[i])
      }
      if((knots[i+degree+1] - knots[i+1]) == 0){
        if(x != knots[i+degree]){
          alpha2 <- 0
        } else {
          return(1)
        }
      } else {
        alpha2 <- (knots[i+degree+1] - x) / (knots[i+degree+1] - knots[i+1])
      }
      B <- alpha1 * basis(x, (degree-1), i, knots) + 
        alpha2*basis(x, (degree-1), (i+1), knots)
    }
    return(B)
  }
  
  # chug them into a matrix
  matrix_b <- function(x, degree=3, int_knots) { 
    # the x argument takes in a vector of time values that 
    # will be used to evaluate a design matrix of basis functions 
    # the degree argument specifies the highest degree of polynomials for
    # the basis functions
    # the int_knots argument takes in a vector of knots that will be used 
    # to determine the intervals of the piecewise function
    bound_knots <- int_knots[c(1, length(int_knots))] # this line creates bound knots
    knots <- c(rep(bound_knots[1], (degree+1)), int_knots[c(-1, -length(int_knots))], rep(bound_knots[2], (degree+1)))
    # the line above adds a couple of extra knots to each end of the int_knots vector because of the Cox-deBoor recursion
    K <- length(int_knots) + degree - 1 # number of columns in the Basis matrix
    B.mat <- matrix(0,nrow = length(x), ncol = K) # initialize the matrix
    for(j in 1:K) {
      B.mat[,j] <- sapply(X = x, FUN = basis, degree = degree, i = j, knots = knots) # add each column, one by one
    }
    return(B.mat) # return the matrix
  }
  
  # this function interpolates a single yield curve 
  interp_yc <- function(yield_list, int_knots, degree = 3, d, last_tenor){
    # yield_list: parameter of the form of a list of data frames containing ZCB spot rate
    # int knots: the interior knots used for b-spline construction
    # degree: highest degree of polynomials for the basis functions
    # d: the date chosen to interpolate from the list
    # last_tenor: last tenor to interpolate in a day
    yield_list[[d]] <- data.frame(Maturity = yield_list[[d]]$Maturity,
                                  ZERO_YLD1 = yield_list[[d]]$ZERO_YLD1)
    yc_df_pre <- rbind(data.frame(Maturity = 0, ZERO_YLD1 = 0), na.omit(yield_list[[d]]))
    last_row <- which(round(yc_df_pre$Maturity,3) == last_tenor)
    yc_df <- yc_df_pre[1:last_row,]
    yields <- c(0, yc_df$ZERO_YLD1)
    maturities <- c(0, as.numeric(yc_df$Maturity))
    x <- as.numeric(maturities) # maturity dates
    B <- matrix_b(x, degree=degree, int_knots = int_knots) 
    B_t_B <- t(B) %*% B
    # B is the design matrix on which the least squares coefficients will be calculated
    
    alphas <- solve(B_t_B) %*% t(B) %*% yields # OLS Formula for coefficients
    x2 <- seq(1/12, last_tenor, 1/12) # this range is used to simulate a continuous yield curve
    B2 <- matrix_b(x2, degree = degree, int_knots = int_knots) 
    # B2 is the matrix of basis functions but evaluated at a 'continuous' time (not really but close enough)
    
    interpolated_yields <- data.frame(Maturity = x2, ZERO_YLD1 = B2 %*% alphas) # create dataframes for plotting
    og_yields <- data.frame(ttm = maturities, yield = yields)
    
    return(interpolated_yields)
  }
  
  # this function takes an entire list and interpolates it
  interpolate_list <- function(yield_list, start, T_, degree = 3){
    # yield_list: Parameter of the form of a list of data frames containing ZCB spot rate
    # start: starting date from the yield_list list
    # T_: length of time window
    # degree: highest degree of polynomials for the basis functions
    interpolated_yc <- list()
    k <- 1
    for(i in start:(start + T_ - 1)){
      lt_max <- max(yield_list[[i]]$Maturity) # This line of code basically chops all yields beyond 20
      avail_ylds <- na.omit(yield_list[[i]]$ZERO_YLD1)
      maturities <- yield_list[[i]]$Maturity
      N <- length(avail_ylds)
      if(N %in% c(5, 6)){
        int_knots <- c(0, quantile(maturities, probs = c(0, 0.5, 1)))
      } else if(N %in% c(7,8,9)){
        int_knots <- c(0,quantile(maturities, probs = c(0, 0.33, 0.66, 1)))
      } else if(N %in% 10:15){
        int_knots <- c(0,quantile(maturities, probs = c(0, 0.25, 0.5, .75, 1)))
      } else {
        int_knots <- c(0,quantile(maturities, probs = c(0, 0.20, 0.4, .6, .8, 1)))
      }
      interpolated_yc[[k]] <- interp_yc(yield_list = yield_list,
                                        int_knots = int_knots,
                                        d = i,
                                        last_tenor = lt_max,
                                        degree = degree)[3:240,]
      k <- k + 1
    }
    return(interpolated_yc)
  }
  
  totNumObs <- length(yieldList)
  numObsInterp <- ifelse(totNumObs > 200, 200, totNumObs)
  
  # cubic spline interp.
  yieldsInterp <- interpolate_list(yieldList, totNumObs - numObsInterp + 1, numObsInterp)
  
  # fit static nelson siegel function
  fit_nelson_siegel <- function(yield_list, lambda, start, tenors, T_){
  # yield_list: Parameter of the form of a list of data frames containing ZCB spot rate
  # lambda: Individual lambda parameter for NS
  # start: starting date from the yield_list list
  # tenors: list of time to maturities
  # T_: length of time window

  indices <- which(round(yield_list[[start]]$Maturity, 2) %in% round(tenors, 2))
  maturities <- yield_list[[1]]$Maturity[indices]
  N <- length(maturities)
  
  term1 <- 1
  term2 <- (1 - exp(-maturities*lambda)) / (maturities*lambda)
  term3 <- ((1 - exp(-maturities*lambda)) / (maturities*lambda)) - exp(-maturities*lambda)
  Phi <- cbind(term1, term2, term3) # Construct Phi matrix for NS
  
  Y_mat <- matrix(0, nrow = N, # matrix of N by T, containing yields for each tenor (columns)
                  ncol = T_) # where each column represents a different date
  j <- 1
  for(t in start:(start + T_- 1)){
    Y_mat[,j] <- yield_list[[t]]$ZERO_YLD1[indices]
    phitphi_1phit <- solve(t(Phi) %*% Phi, t(Phi)) # OLS for the coefficients for every single day
    betas_t <- phitphi_1phit %*% Y_mat  
    betas <- rowSums(betas_t) / T_ # average all coefficients
    eps <- matrix(0, nrow = N, ncol = T_) # matrix of errors for each day (column) and each tenor (row)
    for(t in 1:T_){
      eps[,t] <- Y_mat[,t] - Phi %*% betas # Populate errors
    }
    sig_hat2 <- sum(as.vector(eps)^2) / (N * T_ - 3) # take mean squared error (MLE Estimator)
  }

  return(list(betas = betas, # fitted betas static: 1*3   dynamic: T*3
              sigma2 = sig_hat2, # MSE
              lambda = lambda, # lambda(input)
              cov_mat_betas = sig_hat2 * solve(t(Phi) %*% Phi), # Nelson Siegel design matrix N*3
              eps = eps,
              Phi = Phi)) # residuals N*T
  }
  
  # create time series of betas
  tsBetas <- matrix(NA, nrow = 3, ncol = numObsInterp)
  for (i in 1:numObsInterp) {
    tsBetas[,i] <- fit_nelson_siegel(yieldsInterp,
                                     0.33, i, c(1, 5, 10, 15, 20), 1)$betas
  }
  
  get_C <- function(lambda, tenors) {
    
    # Compute basis functions
    B0 <- rep(1, length(tenors))
    B1 <- (1 - exp(-lambda * tenors)) / (lambda * tenors)
    B2 <- B1 - exp(-lambda * tenors)
    C_matrix <- cbind(B0, B1, B2)
    
    return(C_matrix)
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
  
  # parameters 
  A <- diag(3)
  # diag(A) <- c(yule_walker_ar1(tsBetas[1,])$phi,
  #              yule_walker_ar1(tsBetas[2,])$phi,
  #              yule_walker_ar1(tsBetas[3,])$phi)
  
  diag(A) <- c(0.95,0.9,0.8)
  
  Q <- diag(3)
  diag(Q) <- c(yule_walker_ar1(tsBetas[1,])$sigma2,
               yule_walker_ar1(tsBetas[2,])$sigma2,
                yule_walker_ar1(tsBetas[3,])$sigma2)
  
  R <- diag(length(tenorNum)) * fit_nelson_siegel(yieldsInterp, 
                                                  0.33, i, 
                                                  c(1, 5, 10, 15, 20), 1)$sigma2
  
  C <- get_C(0.33, tenorNum)
  
  icpt <- (diag(3) - A) %*% rowMeans(tsBetas)
  
  KF_loop <- function(A, B, C, D, R, Q, icpt, yields, T_,beta_values) {
    # Preallocate list for observations (assumes yields is a vector or similar)
    y_t <- vector("list", T_)
    
    for (t in 1:T_) {
      y_t[[t]] <- as.matrix(yields[[t]]$ZERO_YLD1)  
    }
    
    # Initial state (x_{0|0}) and covariance (Sigma_{0|0})
    last_x <- matrix(c(5, -3, 3), nrow = 3, ncol = 1) # initialize at OLS
    last_Sig <- diag(3)
    
    # Preallocate matrices to store expected observations and actual observations.
    # Here, we assume the observation dimension is determined by the rows of C.
    n_y <- nrow(C)
    E_y_vec <- matrix(0, nrow = 10, ncol = T_)
    y_real <- matrix(0, nrow = 10, ncol = T_)
    E_x_vec <- matrix(0, nrow = 3, ncol = T_)
    x_real <- matrix(0, nrow = 3, ncol = T_)
    x_var_pred <- list()
    x_var_real <- list()
    xStates <- matrix(0, nrow = 3, ncol = T_)
    
    
    lower_bound <- matrix(0, nrow = 10, ncol = T_)
    upper_bound <- matrix(0, nrow = 10, ncol = T_)
    
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
  
  
  kfRun <- KF_loop(A, diag(3), C, diag(ncol(yields)), R, Q, icpt, yieldList, numObsInterp, tsBetas)

  states <- kfRun$xStates
  covs <- kfRun$x_var_real

  obsCov <- kfRun$obsCov
  baseCov <- kfRun$baseCov

  lastIdx <- ncol(states)
  
  lastState <- states[,lastIdx]
  lastCov <- covs[[lastIdx]]
  lastDate <- dates[lastIdx]
  
  outPut <- list(A,lastState,lastCov,obsCov,baseCov,lastDate)
  
  hStep <- as.numeric(as.Date(valDate) - as.Date(outPut[[6]]))
  
  C <- NS(tenors,0.33)
  A <- outPut[[1]]
  
  foreEX <- C %*% (A %^% hStep) %*% outPut[[2]]
  
  foreVAR <-  (A %^% hStep) %*% outPut[[3]] %*% t(A %^% hStep) + outPut[[5]]
  for(i in 1:(hStep - 1)){
    foreVAR <- foreVAR + (A %^% i) %*% outPut[[5]] %*% t((A %^% i))
  }
  
  foreVAR <- C %*% foreVAR %*% t(C)
  
  simMat <- matrix(nrow = 1000,ncol = nrow(foreVAR))
  
  for(i in 1:nrow(simMat)){
    simMat[i,] <- as.vector(foreEX + mvrnorm(mu = rep(0,nrow(foreVAR)), Sigma = foreVAR))
  }
  
  return(simMat)
}


# yieldMat <- matrix(0, nrow = 50, ncol = 10)
# yieldMock <- c(1.5, 2.1, 2.4, 
#                2.6, 3, 3.1, 
#                3.15, 3.2, 3.23, 3.23)
# for(i in 1:50) {
#   yieldMat[i,] <- yieldMock + mvrnorm(mu = rep(0, 10), Sigma = 0.01 * diag(10))
# }
# 
# chartenors <- c('1M', '3M', '6M', '1Y',
#            '2Y', '3Y', '5Y', '7Y', 
#            '10Y', '20Y')
# colnames(yieldMat) <- chartenors
# 
# dates <- seq(as.Date("1900-01-01"), by = "day", length.out= 50)
# 
# 
# rownames(yieldMat) <- as.character(dates)


#
#




