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

bp <- bondPortfolio(currDate = '2016-01-01')

bond <- bondType(couponRate = 0.05,
                     issueDate = "2015-01-01",
                     maturityDate = "2033-01-01",
                     period = 2,
                     countingConvention = "30/360",
                     bondID = 1,
                     bondSchedule = list(),
                     callable = F)

iniCash <- cashPosition(surplus = 10000, deficit = 0, 
                        fixedRate = log(1.05 ^ (1 / 365)))
bp$setCashPosOb(iniCash)

myLedge <- bondLedger()
bp$setBondLedgerOb(myLedge)


yieldMat <- as.matrix(na.omit(as.data.frame(gbu["2012-01-01::2016-01-01"])))

chartenors <- c('1M', '3M', '6M', '1Y',
                '2Y', '3Y', '5Y', '7Y', 
                '10Y', '20Y')
colnames(yieldMat) <- chartenors

# yo <- yieldObj(realYields = yieldMat, yieldTenors = c(1 / 12, 3 / 12, 6 / 12, 1, 2, 3, 5, 7, 10, 20))
#########################################################
# yo$computeOutKF()
#########################################################

bp$setYieldObj(yo)


bp$bondUpdate('buy',numUnits = 10, bondType = bond, moveForward = FALSE, notChecked = TRUE)


bp$getBondLedgerOb()$getActTable()
bp$getBondLedgerOb()$getTranTable()

############################################################################################

tempOut <- yo$getOutKF()
yo$setOutKF(tempOut)





######################################################################################
# myLen = 100


someOutPut <- yo$simTen('2016-02-01',tenors[-length(tenors)])


yo <- yieldObj(realYields = yieldMat, yieldTenors = c(1 / 12, 3 / 12, 6 / 12, 1, 2, 3, 5, 7, 10, 20))
yo$setOutKF(tempOut)
otherOutPut <- yo$yieldInS('2015-12-31',tenors[-length(tenors)])

plot(tenors[-length(tenors)],gbu['2016-02-03'])
lines(tenors[-length(tenors)],yo$getOutKF()[[7]] %*% (yo$getOutKF()[[1]] %^% 4) %*% yo$getOutKF()[[2]] )

plot(tenors[-length(tenors)],gbu['2016-02-03'])
lines(tenors[-length(tenors)],yo$getOutKF()[[7]] %*% yo$getOutKF()[[2]] )


yo$getOutKF()[[6]]
