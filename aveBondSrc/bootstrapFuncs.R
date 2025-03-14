interp_yc <- function(ttms, yieldMat,newTen, degree = 3){
  
  # yield_list: parameter of the form of a list of data frames containing ZCB spot rate
  # int knots: the interior knots used for b-spline construction
  # degree: highest degree of polynomials for the basis functions
  # d: the date chosen to interpolate from the list
  # last_tenor: last tenor to interpolate in a day
  
  # Initialize Funcs
  
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

  int_knots <- ttms
  N <- length(ttms)
  if(N %in% c(5, 6)){
    int_knots <- c(0, quantile(ttms, probs = c(0, 0.5, 1)))
  } else if(N %in% c(7,8,9)){
    int_knots <- c(0,quantile(ttms, probs = c(0, 0.33, 0.66, 1)))
  } else if(N %in% 10:15){
    int_knots <- c(0,quantile(ttms, probs = c(0, 0.25, 0.5, .75, 1)))
  } else {
    int_knots <- c(0,quantile(ttms, probs = c(0, 0.20, 0.4, .6, .8, 1)))
  }
  


  if((1 %in% dim(yieldMat)) | is.vector(yieldMat)){
    yields <- t(c(0,yieldMat))
  }else{
    yields <- cbind(rep(0, nrow(yieldMat)),yieldMat)
  }
  
  x <- as.numeric(c(0,ttms)) # maturity dates
  B <- matrix_b(x, degree=degree, int_knots = ,int_knots)
  B_t_B <- t(B) %*% B
  
  # B is the design matrix on which the least squares coefficients will be calculated
  
  alphas <- solve(B_t_B) %*% t(B) %*% t(yields) # OLS Formula for coefficients
  x2 <- newTen # this range is used to simulate a continuous yield curve
  B2 <- matrix_b(x2, degree = degree, int_knots = int_knots) 
  # B2 is the matrix of basis functions but evaluated at a 'continuous' time (not really but close enough)
  
  interpolated_yields <- t(B2 %*% alphas) # create dataframes for plotting

  return(interpolated_yields)
}
