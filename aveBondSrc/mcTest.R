source("aveBondSrc/bondPortfolio(IP).R")
source("yieldsMC.R")
source("aveBondSrc/bondType.R")

bp <- bondPortfolio()

bond <- bondType(couponRate = 0.05,
                     issueDate = "2023-01-01",
                     maturityDate = "2033-01-01",
                     period = 2,
                     countingConvention = "30/360",
                     bondID = 0,
                     bondSchedule = list(''),
                     callable = F)
######################################################################################
myLen = 335

yieldMat <- matrix(0, nrow = myLen, ncol = 10)
yieldMock <- c(1.5, 2.1, 2.4, 
               2.6, 3, 3.1, 
               3.15, 3.2, 3.23, 3.23) + 4


for(i in 1:myLen) {
  yieldMat[i,] <- yieldMock + mvrnorm(mu = rep(0, 10), Sigma = 0.01 * diag(10))
}

chartenors <- c('1M', '3M', '6M', '1Y',
                '2Y', '3Y', '5Y', '7Y', 
                '10Y', '20Y')
colnames(yieldMat) <- chartenors

dates <- seq(as.Date("2023-01-01"), by = "day", length.out= myLen)
rownames(yieldMat) <- as.character(dates)


# yieldMC


.yieldMC(yieldMat)

# hStep <- as.numeric(as.Date(valDate) - as.Date(outPut[[6]]))
# 
# C <- NS(tenors,0.33)
# A <- outPut[[1]]
# 
# foreEX <- C %*% (A %^% hStep) %*% outPut[[2]]
# 
# foreVAR <-  (A %^% hStep) %*% outPut[[3]] %*% t(A %^% hStep) + outPut[[5]]
# for(i in 1:(hStep - 1)){
#   foreVAR <- foreVAR + (A %^% i) %*% outPut[[5]] %*% t((A %^% i))
# }
# 
# foreVAR <- C %*% foreVAR %*% t(C)
# 
# simMat <- matrix(nrow = 1000,ncol = nrow(foreVAR))
# 
# for(i in 1:nrow(simMat)){
#   simMat[i,] <- as.vector(foreEX + mvrnorm(mu = rep(0,nrow(foreVAR)), Sigma = foreVAR))
# }
# 
# return(simMat)



######################################################################################


# someThing <- bp$.getAssetPV('2024-01-01',.yieldMC,yieldMat,bond,5,F)
