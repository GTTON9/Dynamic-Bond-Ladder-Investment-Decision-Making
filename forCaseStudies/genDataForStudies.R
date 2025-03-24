

generateYields <- function(state, tenors, lambda, A, Q, R, startDate, endDate, randInvert = F, byeWeekend = F) {
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  if(endDate <= startDate) {
    stop('End date must be at least one day after start date')
  }
  # State: The initial state under which the data will be simulated
  # Q: The covariance of the state
  # R: The covariance of the observation
  # startDate: start date
  # endDate: end date
  # randInvert: If TRUE, the yield curve has a 50% prob of inverting every new year
  # byeWeekend: If TRUE, only yields for week days are calculated
  icpt <- (diag(3) - A) %*% state
  tenors <- sort(tenors)
  N <- length(tenors)
  cMat <- cbind(rep(1, N),
                 (1-exp(-lambda * tenors)) / (lambda * tenors),
                 (1-exp(-lambda * tenors)) / (lambda * tenors) - exp(-lambda * tenors))
  timeLength <- as.numeric( endDate - startDate) 
  dates <- seq(startDate, endDate, 1)
  
  isJan1 <- function(date) {
    date <- as.Date(date)  # Convert to Date format
    return(format(date, "%d") == "01")
  }
  
  
  tsBetas <- matrix(nrow = 3, ncol = timeLength + 1)
  tsSwitch <- matrix(nrow = 3, ncol = timeLength + 1)
  
  tsBetas[,1] <- state
  tsSwitch[,1] <- state
  if (state[2] < 0) {
    switchFactor <- 0.5
  } else {
    switchFactor <- 2
  }
  switch <- F
  for (i in 2:(timeLength+1)) {
    tsBetas[,i] <-icpt + A %*% tsBetas[,i-1] + mvrnorm(mu = rep(0, 3), Sigma = Q)
    if (switch) {
      tsSwitch[,i] <- tsBetas[,i] * c(switchFactor, -1, 1)
    } else {
      tsSwitch[,i] <- tsBetas[,i]
    }
    if(isJan1(dates[i]) && randInvert) { # randomly invert the curve 
      switch <- as.logical(rbinom(1,1,0.5))
      }
    }


  yields <- cMat %*% tsSwitch
  
  # get rid of weekends if desired
  if (byeWeekend) {
    bizDays <- which(!sapply(X = dates, FUN = weekdays) %in% c('Saturday', 'Sunday'))
    dates <- dates[bizDays]
    yields <- yields[,bizDays]
  }
  yields <- t(yields)
  rownames(yields) <- as.character(dates)
  return(yields)
}
# seq(as.Date('2000-01-01'), as.Date('2000-01-30'), 1)
# 
# yields <- generateYields(state = c(5, 3, -3), 
#                          tenors = c(1, 5, 10, 15, 20), 
#                          lambda = 0.33, 
#                          A = diag(c(0.9, 0.8, 0.7)), 
#                          Q = 0.01 * diag(3),
#                          R = 0.02 * diag(5), 
#                          startDate = "2001-01-01", 
#                          endDate = '2002-01-01', 
#                          randInvert = T,
#                          byeWeekend = T)
# 
# yields[100,]

