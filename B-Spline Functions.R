bRecur <- function(t,i,k,knots){
    
  if(k == 0){
      
      if((knots[i] <= t) & (t < knots[i+1])){
        
        return(1)
        
      }
      else{
        
        return(0)
        
      }
    }

  else{
    
    if(knots[i + k] - knots[i] == 0){
      if(t != knots[i + k]){
      part1 <- 0}
      else{
        return(1)
      }
    }
    else{
      part1 <- (t - knots[i]) / (knots[i+k] - knots[i]) * bRecur(t,i,k-1,knots)
    }
    if((knots[i+k+1] - knots[i+1]) == 0){
      if(t != knots[i+k+1]){
      part2 <- 0}
      else{
        return(1)
      }
    }
    else{
      part2 <- (knots[i+k+1] - t) / (knots[i+k+1] - knots[i+1]) * bRecur(t,i+1,k-1,knots)
    }
    
    return(part1 + part2) 
              
  }

}

bSpline <- function(tVec,knots){
  
  bound_knots <- knots[c(1, length(knots))] 
  allKnots <- c(rep(bound_knots[1], (3+1)), knots[c(-1, -length(knots))], 
             rep(bound_knots[2], (3+1)))
  
  colDim <- length(knots) + 3 - 1
  
  bMat <- matrix(data = NA,nrow = length(tVec),ncol = colDim)
  
  for(j in (1:length(tVec))){
    for(i in 1:colDim){
      bMat[j,i] <- bRecur(tVec[j],i,3,allKnots)
    }
  }
  return(bMat)
}


# x <- rnorm(n = 100)
# y <- rnorm(n = 100) + 1

# x <- mtcars$mpg
# y <- mtcars$disp
# 
# plot(x,y)
# myKnots <- quantile(x,c(0,0.25,0.5,0.75,1))
# 
# someVec <- bSpline(x,myKnots)
# 
# regX <- seq(from = 10.4,to = 35,by = 0.1555555)
# 
# xVec <- bSpline(regX,myKnots)
# 
# 
# yPred <- xVec %*% solve(t(someVec) %*% someVec,diag(ncol(someVec))) %*% t(someVec) %*% y
# 
# lines(regX,yPred)

