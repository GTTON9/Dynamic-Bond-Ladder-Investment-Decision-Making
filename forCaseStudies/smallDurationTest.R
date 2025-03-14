source("aveBondSrc/bondPortfolio(IP).R")
source("aveBondSrc/cashPosition.R")
source("aveBondSrc/bondLedger.R")
source("aveBondSrc/bondType.R")
source("aveBondSrc/yieldObj.R")

# source("yieldsMC.R")
source("aveBondSrc/phoronYield.R")

library(ggplot2)
library(expm)
library(pdfetch)
library(xts)

if(!exists("gbu")){
  gbu <- pdfetch_FRED(c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1",
                        "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20"))
  }

# bp <- bondPortfolio(currDate = '2015-12-31')
bp <- bondPortfolio(currDate = '2022-01-05')

bond <- bondType(couponRate = 0,
                 issueDate = "2021-06-01",
                 maturityDate = "2022-06-01",
                 period = 0,
                 countingConvention = "30/360",
                 bondID = 1,
                 bondSchedule = list(),
                 callable = F)


startDate <- as.Date("2022-01-01") + 30
endDate <- as.Date("2023-01-01")

makeSched <- seq(startDate,endDate,length.out = 4)
coupBond <- bondType(couponRate = 0.05,
                     issueDate = "2022-01-01",
                     maturityDate = "2023-01-01",
                     period = 4,
                     countingConvention = "30/360",
                     bondID = 2,
                     bondSchedule = makeSched,
                     callable = F)

iniCash <- cashPosition(surplus = 10000, deficit = 0, 
                        fixedRate = log(1.0 ^ (1 / 365)))
bp$setCashPosOb(iniCash)

myLedge <- bondLedger()
bp$setBondLedgerOb(myLedge)


# yieldMat <- as.matrix(na.omit(as.data.frame(gbu["2012-01-01::2016-01-04"])))
yieldMat <- as.matrix(gbu["2020-01-01::2022-01-05"])
yieldMat <- yieldMat[rowSums(is.na(yieldMat)) != ncol(yieldMat),]


chartenors <- c('1M', '3M', '6M', '1Y',
                '2Y', '3Y', '5Y', '7Y', 
                '10Y', '20Y')
colnames(yieldMat) <- chartenors

yo <- yieldObj(realYields = yieldMat, yieldTenors = c(1 / 12, 3 / 12, 6 / 12, 1, 2, 3, 5, 7, 10, 20))
#########################################################
yo$computeOutKF()
#########################################################


bp$setYieldObj(yo)


bp$bondUpdate('buy',numUnits = 1000, bondType = bond, moveForward = FALSE, notChecked = TRUE)
bp$bondUpdate('buy',numUnits = 9000, bondType = coupBond, moveForward = FALSE, notChecked = TRUE)


# bp$bondUpdate('sell', bondType = bond, moveForward = FALSE, notChecked = TRUE)
tsDF <- as.data.frame(matrix(ncol = 4,nrow = 0))
colnames(tsDF) <- c('Date','PFV_low','PFV_mean',"PFV_up")

while(as.Date(bp$getCurrDate()) != as.Date('2022-01-30')){
  bp$bondUpdate('none',moveForward = TRUE)
  
  portVals <- bp$getPortVal()
  
  pNames <- names(tsDF)
  
  #  / sqrt(length(portVals))
  currSE <- sd(portVals) * pnorm(0.975)
  currMean <- mean(portVals)
  currLwr <- currMean - currSE * pnorm(0.975)
  currUpr <- currMean + currSE * pnorm(0.975)
  
  tsDF <-  rbind(tsDF,list(bp$getCurrDate(),currLwr,currMean,currUpr))
  
  names(tsDF) <- pNames
  
}



ggplot(data = tsDF, aes(x = as.Date(Date),y = PFV_mean)) + geom_line() + 
  geom_ribbon(aes(ymin = PFV_low, ymax = PFV_up),alpha=0.2)