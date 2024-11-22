library(pdfetch)

source('B-Spline Functions.R')

# some more functions

penalRSS <- function(x,y,s){
  H <- x %*% (solve(s * diag(ncol(x)) + t(x) %*% x)) %*% t(x)
  
  resid <- (diag(nrow(x)) - H) %*% y
  
  return(t(resid) %*% resid)
}

penalBeta <- function(x,y,s){
  return((solve(s * diag(ncol(x)) + t(x) %*% x)) %*% t(x) %*% y)
}

# Intialize the data

gbu <- pdfetch_FRED(c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1",
                      "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30"))


names(gbu) <- c("1M","3M","6M","1Y","2Y","3Y","5Y","7Y","10Y","20Y","30Y")

gbu <- gbu[rowSums(is.na(gbu)) != ncol(gbu),]

# write.csv(gbu,"C:\\Users\\ander\\Documents\\PSTAT 296\\Data\\Clean_Yields.csv")

tenors <- c(1 / 12,0.25,0.5,1,2,3,5,7,10,20,30)

myKnots <- quantile(tenors,c(0,0.25,0.8,1))
myKnots <- sort(c(myKnots,30))

B <- bSpline(tenors,myKnots)

optFunc <- function(s){
  return(penalRSS(B,t(gbu[nrow(gbu),]),s))
}

s <- optimize(optFunc,c(-1,1))$minimum
betaVec <- penalBeta(B,t(gbu[nrow(gbu),]),s)

mySeq <- seq(1/12,30,length.out = 30)
xVec <- bSpline(mySeq,myKnots)

myPred <- xVec %*% betaVec
# myPred <- B %*% betaVec


plot(tenors,gbu[nrow(gbu),])
lines(mySeq,myPred,col = 'red')
