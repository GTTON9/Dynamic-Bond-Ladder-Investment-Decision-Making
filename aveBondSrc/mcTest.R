source("C:/Users/ander/Documents/PSTAT 296/PSTAT296-Project/aveBondSrc/bondPortfolio(IP).R")
source("C:/Users/ander/Documents/PSTAT 296/PSTAT296-Project/yieldsMC.R")
source("C:/Users/ander/Documents/PSTAT 296/PSTAT296-Project/aveBondSrc/bondType.R")

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



######################################################################################


someThing <- bp$.getAssetPV('2024-01-01',.yieldMC,yieldMat,bond,5,F)
