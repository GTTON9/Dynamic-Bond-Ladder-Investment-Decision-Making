source("aveBondSrc/bondPortfolio(IP).R")
source("aveBondSrc/cashPosition.R")
source("aveBondSrc/bondLedger.R")
source("aveBondSrc/bondType.R")
source("aveBondSrc/yieldObj.R")

source("yieldsMC.R")

library(expm)
library(pdfetch)
gbu <- pdfetch_FRED(c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1",
                      "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20"))

bp <- bondPortfolio(currDate = '2023-12-01')

bond <- bondType(couponRate = 0.05,
                     issueDate = "2023-01-01",
                     maturityDate = "2033-01-01",
                     period = 2,
                     countingConvention = "30/360",
                     bondID = 0,
                     bondSchedule = list(''),
                     callable = F)

iniCash <- cashPosition(surplus = 10000, deficit = 0, 
                        fixedRate = log(1.05 ^ (1 / 365)))
bp$setCashPosOb(iniCash)

myLedge <- bondLedger()
bp$setBondLedgerOb(myLedge)

bp$bondUpdate('buy',numUnits = 10, bondType = bond, moveForward = FALSE, notChecked = TRUE)












######################################################################################
# myLen = 100

yieldMat <- as.matrix(na.omit(as.data.frame(gbu["2012-01-01::2016-01-01"])))

# yieldMat <- matrix(0, nrow = myLen, ncol = 10)
# yieldMock <- c(1.5, 2.1, 2.4, 
#                2.6, 3, 3.1, 
#                3.15, 3.2, 3.23, 3.23) + 4
# 
# 
# for(i in 1:myLen) {
#   yieldMat[i,] <- yieldMock + mvrnorm(mu = rep(0, 10), Sigma = 0.01 * diag(10))
# }

chartenors <- c('1M', '3M', '6M', '1Y',
                '2Y', '3Y', '5Y', '7Y', 
                '10Y', '20Y')
colnames(yieldMat) <- chartenors

# dates <- seq(as.Date("2023-01-01"), by = "day", length.out= myLen)
# rownames(yieldMat) <- as.character(dates)

yo <- yieldObj(realYields = yieldMat)
yo$computeOutKF()
someOutPut <- yo$simTen('2016-02-01',tenors[-length(tenors)])

plot(tenors[-length(tenors)],gbu['2016-02-03'])
lines(tenors[-length(tenors)],yo$getOutKF()[[7]] %*% (yo$getOutKF()[[1]] %^% 4) %*% yo$getOutKF()[[2]] )

plot(tenors[-length(tenors)],gbu['2016-02-03'])
lines(tenors[-length(tenors)],yo$getOutKF()[[7]] %*% yo$getOutKF()[[2]] )


yo$getOutKF()[[6]]
