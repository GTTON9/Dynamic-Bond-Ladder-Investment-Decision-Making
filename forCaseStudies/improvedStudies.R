source("aveBondSrc/bondPortfolio(IP).R")
source("aveBondSrc/cashPosition.R")
source("aveBondSrc/bondLedger.R")
source("aveBondSrc/yieldObj.R")
source('aveBondSrc/phoronYield.R')
source('forCaseStudies/genDataForStudies.R')
source("forCaseStudies/vRecur.R")
source("aveBondSrc/bondType.R")
library(xts)

# allYields <- generateYields(c(4, -3, 2), c(1/12, 1/4, 1/2,
#                                            1,2,3,5,7,10,20),
#                             lambda = 1/3, diag(c(0.9, 0.9, 0.9)),
#                             diag(c(0.01, 0.01, 0.01)),
#                             startDate = '2022-01-01',
#                             endDate = '2026-05-01',randInvert = T)
allYields <- na.omit(as.data.frame(gbu[rowSums(is.na(gbu)) != ncol(gbu),])) # Needs fixing in base code.

tempXts <- xts(allYields, order.by=as.Date(rownames(allYields)))
pastYields <- as.matrix(tempXts["2021-04-01::2023-04-01"])
futureYields <- as.matrix(tempXts["2023-04-02::"])

chartenors <- c('1M', '3M', '6M', '1Y',
                '2Y', '3Y', '5Y', '7Y', 
                '10Y', '20Y')
colnames(pastYields) <- chartenors
colnames(futureYields) <- chartenors

bondList <- list()

startDate <- as.Date("2023-01-01") + 30
endDate <- as.Date("2024-01-01")

makeSched <- seq(startDate,endDate,length.out = 4)
bondList[[1]] <- bondType(couponRate = 0.05,
                          issueDate = "2023-01-01",
                          maturityDate = "2024-01-01",
                          period = 4,
                          countingConvention = "30/360",
                          bondID = 1,
                          bondSchedule = makeSched,
                          callable = F)

startDate <- as.Date("2023-02-01") + 30
endDate <- as.Date("2024-02-01")

makeSched <- seq(startDate,endDate,length.out = 4)
bondList[[2]] <- bondType(couponRate = 0.05,
                          issueDate = "2023-02-01",
                          maturityDate = "2024-02-01",
                          period = 4,
                          countingConvention = "30/360",
                          bondID = 2,
                          bondSchedule = makeSched,
                          callable = F)

startDate <- as.Date("2023-03-01") + 30
endDate <- as.Date("2024-03-01")

makeSched <- seq(startDate,endDate,length.out = 4)
bondList[[3]] <- bondType(couponRate = 0.05,
                          issueDate = "2023-03-01",
                          maturityDate = "2024-03-01",
                          period = 4,
                          countingConvention = "30/360",
                          bondID = 3,
                          bondSchedule = makeSched,
                          callable = F)

commonTenors <- c(1 / 12, 3 / 12, 6 / 12, 1, 2, 3, 5, 7, 10, 20)

por1 <- bondPortfolio(currDate = '2023-04-01')
cash1 <- cashPosition(surplus = 100000, deficit = 0, 
                      fixedRate = log(1.03 ^ (1 / 365)))
por1$setCashPosOb(cash1)
ledge1 <- bondLedger()
por1$setBondLedgerOb(ledge1)

yo1 <- yieldObj(realYields = pastYields, yieldTenors = commonTenors)
kfTemp <- yo1$computeOutKF()
por1$setYieldObj(yo1)


por1$bondUpdate('buy',
                numUnits = 10000,
                bondType = bondList[[1]],
                moveForward = F,
                notChecked = T)

por1$bondUpdate('buy',
                numUnits = 20000,
                bondType = bondList[[2]],
                moveForward = F,
                notChecked = T)
por1$bondUpdate('buy',
                numUnits = 40000,
                bondType = bondList[[3]],
                moveForward = F,
                notChecked = T)

#################################################################################################################

source("aveBondSrc/findPaths.R")

fp <- findPaths(por1,as.character(as.Date(por1$getCurrDate()) + 365))

myList <- fp$walkPaths()
