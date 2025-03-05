stateForecast <- function(A, Q, icpt, h, lastX, lastSig) {
  # A: Transition matrix
  # Q: Covariance of State
  # icpt: Intercept of the state
  # h: steps ahead
  # lastX: last estimate of the state (x_{T|T})
  # lastSig: last estimate of the covariance of the state
  retList <- list()
  listMean <- list()
  listCov <- list()
  lastMean <- lastX
  lastCov <- lastSig
  for(i in 1:h) { 
    newMean <- icpt + A %*% lastMean
    newCov <- A %*% lastCov %*% t(A) + Q
    listMean[[i]] <- newMean
    listCov[[i]] <- newCov
    lastMean <- newMean
    lastCov <- newCov
  }
  level <- c()
  slope <- c()
  curvature <- c()
  up95level <- c()
  lo95level <- c()
  up95slope <- c()
  lo95slope <- c()
  up95curvature <- c()
  lo95curvature <- c()
  for (i in 1:h) {
    # Extract level, slope, and curvature from the listMean
    level[i] <- listMean[[i]][1]
    slope[i] <- listMean[[i]][2]
    curvature[i] <- listMean[[i]][3]
    
    # Calculate 95% confidence intervals for level, slope, and curvature
    # Using 1.96 as the critical value for a 95% confidence interval (assuming normality)
    up95level[i] <- level[i] + sqrt(listCov[[i]][1, 1]) * 1.96
    lo95level[i] <- level[i] - sqrt(listCov[[i]][1, 1]) * 1.96
    
    up95slope[i] <- slope[i] + sqrt(listCov[[i]][2, 2]) * 1.96
    lo95slope[i] <- slope[i] - sqrt(listCov[[i]][2, 2]) * 1.96
    
    up95curvature[i] <- curvature[i] + sqrt(listCov[[i]][3, 3]) * 1.96
    lo95curvature[i] <- curvature[i] - sqrt(listCov[[i]][3, 3]) * 1.96
  }
  print(up95curvature)
  print(lo95curvature)
  
  # Create a data frame to store the forecasted values and their confidence intervals
  fc <- data.frame(
    level = level,
    slope = slope,
    curvature = curvature,
    upper95level = up95level,
    lower95level = lo95level,
    upper95slope = up95slope,
    lower95slope = lo95slope,
    upper95curvature = up95curvature,
    lower95curvature = lo95curvature
  )
  return(fc)
}

for100 <- stateForecast(A_YW, Q, icpt, 30, a$E_x_vec[,100], a$x_var_pred[[100]])
for100level <- data.frame(level = for100$level,
                          lo95level = for100$lower95level,
                          up95level = for100$upper95level)
last100level <- data.frame(level = t(beta_values)[1,],
                           lo95level = NA,
                           up95level = NA)
last100_for100_l <- rbind.data.frame(last100level, for100level)


ggplot() + geom_line(data = last100_for100_l, aes(x = 1:130, y = level)) + geom_ribbon(data = last100_for100_l, aes(x = 1:130, ymin = lo95level, ymax = up95level, alpha = 0.2))

for100slope <- data.frame(slope = for100$slope,
                          lo95slope = for100$lower95slope,
                          up95slope = for100$upper95slope)
last100slope <- data.frame(slope = t(beta_values)[2,],
                           lo95slope = NA,
                           up95slope = NA)
last100_for100_s <- rbind.data.frame(last100slope, for100slope)

ggplot() + geom_line(data = last100_for100_s, aes(x = 1:130, y = slope)) + geom_ribbon(data = last100_for100_s, aes(x = 1:130, ymin = lo95slope, ymax = up95slope, alpha = 0.2))

last100slope <- data.frame(slope = t(beta_values)[2,],
                           lo95slope = NA,
                           up95slope = NA)
last100_for100_s <- rbind.data.frame(last100slope, for100slope)

ggplot() + geom_line(data = last100_for100_s, aes(x = 1:130, y = slope)) + geom_ribbon(data = last100_for100_s, aes(x = 1:130, ymin = lo95slope, ymax = up95slope, alpha = 0.2))