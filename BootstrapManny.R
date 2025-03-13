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

interp_yc <- function(yield_list, int_knots, degree = 3, d, last_tenor, Bprod = NULL){
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
  if (!is.null(Bprod)) {
    BtB1Bt <- Bprod
  } else {
    BtB1Bt <- solve(B_t_B) %*% t(B)
  }
  alphas <- BtB1Bt %*% yields # OLS Formula for coefficients
  x2 <- seq(1/2, last_tenor, 1/2) # this range is used to simulate a continuous yield curve
  B2 <- matrix_b(x2, degree = degree, int_knots = int_knots) 
  # B2 is the matrix of basis functions but evaluated at a 'continuous' time (not really but close enough)
  
  interpolated_yields <- data.frame(Maturity = x2, ZERO_YLD1 = B2 %*% alphas) # create dataframes for plotting
  og_yields <- data.frame(ttm = maturities, yield = yields)
  
  return(list(interpYields = interpolated_yields, Bprod = BtB1Bt))
}





interpolate_list <- function(yield_list, start, T_, degree = 3){
  mat <- matrix(NA, ncol = 10, nrow = T_)
  colnames(mat) <- c("1M", "3M", '6M', 
                     '1Y', '2Y', '3Y', '5Y',
                     '7Y', '10Y', '20Y')
  commonTenors <- c(1/12, 1/4, 1/2,
                    1,2,3,5,7,
                    10,20)
  dates <- c()
  
  newList <- yield_list[start:(start + T_ - 1)]
  for(i in 1:T_) {
    mat[i,] <- newList[[i]]$ZERO_YLD1[1:10]
    dates[i] <- newList[[i]]$START_DT[1]
  }
  print(dates)
  rownames(mat) <- dates
  
  df <- as.data.frame(mat)
  print(df)
  uniqueDF <- as.data.frame(unique(is.na(df)))
  
  listConfigs <- list()
  listIdx <- list()
  for (i in 1:nrow(uniqueDF)) {
    listConfigs[[i]] <- colnames(df)[which(!as.logical(uniqueDF[i,]))]
  }
  
  subset_indices <- function(original_set, subset) {
    # Convert set to vector for index lookup
    original_list <- as.vector(original_set)
    
    # Get indices of subset elements in the original set
    indices <- which(original_list %in% subset)
    
    return(indices)
  }
  
  for (i in 1:nrow(uniqueDF)) {
    sub <- rownames(match_df(as.data.frame(is.na(df)), uniqueDF[i,]))
    listIdx[[i]] <- subset_indices(rownames(df), sub)
  }
  mat <- as.matrix(df)
  #print(listIdx)
  byCatList <- list()
  k <- 1
  changeMats <- c()
  #print(uniqueDF)
  for (i in 1:nrow(uniqueDF)) {
    for (j in listIdx[[i]]) {
      
      if (length(na.omit(mat[j,])) != 0) {
        byCatList[[k]] <- na.omit(data.frame(Maturity = commonTenors,
                                             ZERO_YLD1 = mat[j,],
                                             START_DT = rownames(mat)[j]))
        k <- k + 1
        if (j == min(listIdx[[i]])) { 
          changeMats <- c(changeMats, k-1)
        }
      }
    }
  }
  
  
  
  #####
  
  # yield_list: Parameter of the form of a list of data frames containing ZCB spot rate
  # start: starting date from the yield_list list
  # T_: length of time window
  # degree: highest degree of polynomials for the basis functions
  interpolated_yc <- list()
  k <- 1
  print(changeMats)
  
  for(i in 1:length(byCatList)){
    lt_max <- max(byCatList[[i]]$Maturity) # This line of code basically chops all yields beyond 20
    avail_ylds <- na.omit(byCatList[[i]]$ZERO_YLD1)
    maturities <- byCatList[[i]]$Maturity
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
    if (k %in% changeMats) {
      interpVar <- interp_yc(yield_list = byCatList,
                             int_knots = int_knots,
                             d = i,
                             last_tenor = lt_max,
                             degree = degree)
      
      interpolated_yc[[k]] <- cbind.data.frame(interpVar$interpYields[1:(lt_max * 2),], 
                                               START_DT = byCatList[[i]]$START_DT[1])
      bprod <- interpVar$Bprod
      
    } else {
      interpVar <- interp_yc(yield_list = byCatList,
                             int_knots = int_knots,
                             d = i,
                             last_tenor = lt_max,
                             degree = degree,
                             Bprod = bprod)
      interpolated_yc[[k]] <- cbind.data.frame(interpVar$interpYields[1:(lt_max * 12),], 
                                               START_DT = byCatList[[i]]$START_DT[1])
    }
    
    k <- k + 1
  }
  return(interpolated_yc)
}