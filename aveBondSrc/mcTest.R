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
bp <- bondPortfolio(currDate = '2014-12-31')

bond <- bondType(couponRate = 0,
                     issueDate = "2014-01-01",
                     maturityDate = "2033-01-01",
                     period = 0,
                     countingConvention = "30/360",
                     bondID = 1,
                     bondSchedule = list(),
                     callable = F)


startDate <- as.Date("2014-01-01") + 30
endDate <- as.Date("2033-01-01")

makeSched <- seq(startDate,endDate,length.out = 18 * 2)
coupBond <- bondType(couponRate = 0.05,
                 issueDate = "2014-01-01",
                 maturityDate = "2033-01-01",
                 period = 2,
                 countingConvention = "30/360",
                 bondID = 2,
                 bondSchedule = makeSched,
                 callable = F)

iniCash <- cashPosition(surplus = 10000, deficit = 0, 
                        fixedRate = log(1.5 ^ (1 / 365)))
bp$setCashPosOb(iniCash)

myLedge <- bondLedger()
bp$setBondLedgerOb(myLedge)


# yieldMat <- as.matrix(na.omit(as.data.frame(gbu["2012-01-01::2016-01-04"])))
yieldMat <- as.matrix(gbu["2012-01-01::2015-01-05"])
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
bp$bondUpdate('buy',numUnits = 3000, bondType = coupBond, moveForward = FALSE, notChecked = TRUE)


# '2033-01-02'
while(as.Date(bp$getCurrDate()) != as.Date('2015-01-05')){

  bp$bondUpdate('none',moveForward = TRUE)

}


# bp$bondUpdate('sell', bondType = coupBond, moveForward = FALSE, notChecked = TRUE)
tsDF <- as.data.frame(matrix(ncol = 4,nrow = 0))
colnames(tsDF) <- c('Date','PFV_low','PFV_mean',"PFV_up")

while(as.Date(bp$getCurrDate()) != as.Date('2015-01-31')){
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




# bp$yieldObj$appRealYields(as.matrix(na.omit(gbu['2016-01-01::2016-01-04'])))
tempOut <- bp$getYieldObj()$getOutKF()
# bp$yieldObj$computeOutKF()


bp$yieldObj$setOutKF(tempOut)
bp$bondUpdate('strip', bondType = coupBond, moveForward = FALSE, notChecked = TRUE)


bp$bondUpdate('sell', bondType = bond, moveForward = FALSE, notChecked = TRUE)

# Moving forward : works for cash. Now for getAssetValue


bp$bondUpdate('none',moveForward = TRUE)
# bp$yieldObj$appRealYields(gbu[])



# Check the biz


bp$getBondLedgerOb()$getActTable()
bp$getBondLedgerOb()$getTranTable()
bp$getCashPos()
bp$getPortVal()

bp$getCashPosOb()$getDeficit()
bp$getCashPosOb()$getSurplus()

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

##########################################################################################

# Secret ops

result <- funcW(list(bp),currDate = bp$getCurrDate(),endDate = '2015-01-31')
