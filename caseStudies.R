source("aveBondSrc/bondPortfolio(IP).R")
source("aveBondSrc/cashPosition.R")
source("aveBondSrc/bondLedger.R")
source("aveBondSrc/bondType.R")
source("aveBondSrc/yieldObj.R")
source('aveBondSrc/phoronYield.R')
source('forCaseStudies/genDataForStudies.R')
library(xts)

allYields <- generateYields(c(4, -3, 2), c(1/12, 1/4, 1/2,
                                           1,2,3,5,7,10,20),
                            lambda = 1/3, diag(c(0.9, 0.9, 0.9)),
                            diag(c(0.01, 0.01, 0.01)),
                            startDate = '2022-01-01',
                            endDate = '2025-04-30')

tempXts <- xts(allYields, order.by=as.Date(rownames(allYields)))
pastYields <- as.matrix(tempXts["::2025-04-01"])
futureYields <- as.matrix(tempXts["2025-04-02::"])
colnames(pastYields) <- chartenors
colnames(futureYields) <- chartenors

por1 <- bondPortfolio(currDate = '2025-04-01')
bondList <- list()

bondList[[1]] <- bondType(
  couponRate = 0.03, 
  issueDate = '2025-04-01',
  maturityDate = '2026-04-01',
  period = 2,
  countingConvention = '30/360',
  callable = T,
  bondID = 1,
  bondSchedule = c('2025-10-01', '2026-04-01')
)

bondList[[2]] <- bondType(
  couponRate = 0.03, 
  issueDate = '2025-05-01',
  maturityDate = '2026-05-01',
  period = 2,
  countingConvention = '30/360',
  callable = T,
  bondID = 2,
  bondSchedule = c('2025-11-01', '2026-05-01')
)

bondList[[3]] <- bondType(
  couponRate = 0.03, 
  issueDate = '2025-06-01',
  maturityDate = '2026-06-01',
  period = 2,
  countingConvention = '30/360',
  callable = T,
  bondID = 1,
  bondSchedule = c('2025-12-01', '2026-06-01')
)



por1 <- bondPortfolio(currDate = '2025-04-01')
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

por1$bondUpdate('none')


