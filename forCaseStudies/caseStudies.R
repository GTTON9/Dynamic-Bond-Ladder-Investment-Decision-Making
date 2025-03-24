source("aveBondSrc/bondPortfolio(IP).R")
source("aveBondSrc/cashPosition.R")
source("aveBondSrc/bondLedger.R")
source("aveBondSrc/yieldObj.R")
source('aveBondSrc/phoronYield.R')
source('forCaseStudies/genDataForStudies.R')
source("forCaseStudies/vRecur.R")
source("aveBondSrc/bondType.R")
library(xts)

allYields <- generateYields(c(4, -3, 2), c(1/12, 1/4, 1/2,
                                           1,2,3,5,7,10,20),
                            lambda = 1/3, diag(c(0.9, 0.9, 0.9)),
                            diag(c(0.01, 0.01, 0.01)),
                            startDate = '2022-01-01',
                            endDate = '2026-05-01',randInvert = T)

tempXts <- xts(allYields, order.by=as.Date(rownames(allYields)))
pastYields <- as.matrix(tempXts["::2025-04-01"])
futureYields <- as.matrix(tempXts["2025-04-02::"])

chartenors <- c('1M', '3M', '6M', '1Y',
                '2Y', '3Y', '5Y', '7Y', 
                '10Y', '20Y')
colnames(pastYields) <- chartenors
colnames(futureYields) <- chartenors

por1 <- bondPortfolio(currDate = '2025-04-01')
bondList <- list()

startDate <- as.Date("2025-01-01") + 30
endDate <- as.Date("2026-01-01")

makeSched <- seq(startDate,endDate,length.out = 4)
bondList[[1]] <- bondType(couponRate = 0.05,
                     issueDate = "2025-01-01",
                     maturityDate = "2026-01-01",
                     period = 4,
                     countingConvention = "30/360",
                     bondID = 1,
                     bondSchedule = makeSched,
                     callable = F)

startDate <- as.Date("2025-02-01") + 30
endDate <- as.Date("2026-02-01")

makeSched <- seq(startDate,endDate,length.out = 4)
bondList[[2]] <- bondType(couponRate = 0.05,
                          issueDate = "2025-02-01",
                          maturityDate = "2026-02-01",
                          period = 4,
                          countingConvention = "30/360",
                          bondID = 2,
                          bondSchedule = makeSched,
                          callable = F)

startDate <- as.Date("2025-03-01") + 30
endDate <- as.Date("2026-03-01")

makeSched <- seq(startDate,endDate,length.out = 4)
bondList[[3]] <- bondType(couponRate = 0.05,
                          issueDate = "2025-03-01",
                          maturityDate = "2026-03-01",
                          period = 4,
                          countingConvention = "30/360",
                          bondID = 2,
                          bondSchedule = makeSched,
                          callable = F)

commonTenors <- c(1 / 12, 3 / 12, 6 / 12, 1, 2, 3, 5, 7, 10, 20)

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

begDate <- por1$getCurrDate()
endDate <- as.character(as.Date(begDate) + 365)
bpSched <- seq(as.Date(begDate),as.Date(endDate),1)
invSched <- seq(as.Date(begDate),as.Date(endDate),12)

portDF <- as.data.frame(matrix(nrow = 365,ncol = 2))
colnames(portDF) <- c('Date','portVal')

nadaDF <- as.data.frame(matrix(nrow = 365,ncol = 2))
colnames(nadaDF) <- c('Date','portVal')

nadaPort <- por1$copy()

idCount <- 3

for(i in 1:length(bpSched)){
  print('working')
  
  if(bpSched[i] != as.Date(por1$getCurrDate())){
    stop('Houston we have a problem')
  }
  
  currTermin <- invSched[invSched > bpSched[i]][1]
  newPor <- por1$copy()
  currIDs <- por1$bondLedger$getActTable()$bondID
  
  if(is.na(currTermin)){
    newDate <- lubridate::ceiling_date(bpSched[i] + 1,unit = 'months') - 1
    
    newBond <- bondType(
      couponRate = 0.03, 
      issueDate = as.character(bpSched[i]),
      maturityDate = as.character(as.Date(bpSched[i]) + 365),
      period = 4,
      countingConvention = '30/360',
      callable = T,
      bondID = idCount + 1,
      bondSchedule = as.character(seq(as.Date(newDate),as.Date(newDate) + 365,length.out = 4))
    )
    idCount <- idCount + 1
    
    currVal <- mean(por1$getPortVal())
    portDF[i,] <- c(as.character(bpSched[i]),currVal)
    nadaVal <- nadaPort$getPortVal()
    nadaDF[i,] <- c(as.character(bpSched[i]),nadaVal)
    por1$bondUpdate('buy',numUnits = 10, bondType = newBond, moveForward = TRUE, notChecked = TRUE)
    nadaPort$bondUpdate('buy',numUnits = 10, bondType = newBond, moveForward = TRUE, notChecked = TRUE)
    
    next
  }
  
  
  if(!is.null(currIDs)){
  
    for(j in currIDs){
      result <- vRecur(por1,currDate = por1$getCurrDate(),terminalDate = as.character(currTermin),j)
      action <- result$bestAction
      bondType <- result$bondType
      print(result$currDate)
      
      if(!is.na(action)){
        por1$bondUpdate(action, bondType = bondType, moveForward = FALSE, notChecked = TRUE)
        break
      }
    }
    currVal <- mean(por1$getPortVal())
    portDF[i,] <- c(as.character(bpSched[i]),currVal)
    
    por1$bondUpdate('none',moveForward = TRUE)
  }else{
    por1$bondUpdate('none',moveForward = TRUE)
  }
  
  
  nadaVal <- nadaPort$getPortVal()
  nadaDF[i,] <- c(as.character(bpSched[i]),nadaVal)
  nadaPort$bondUpdate('none',moveForward = TRUE)
  
  nadaPort$yieldObj$appRealYields(futureYields[i,])
  por1$yieldObj$appRealYields(futureYields[i,])
  por1$yieldObj$computeOutKF()
  
}


library(ggplot2)
library(ggthemes)

nadaDF <- na.omit(nadaDF)
portDF <- na.omit(portDF)

mergeDF <- cbind(nadaDF,portDF[,2])
colnames(mergeDF) <- c('Date','Double_Stationary','Action_Stationary')
mergeDF[,2] <- as.numeric(mergeDF[,2])
mergeDF[,3] <- as.numeric(mergeDF[,3])

ggplot(mergeDF,aes(x = as.Date(Date),y = Double_Stationary)) + geom_line(linewidth = 1) + 
  geom_line(aes(x = as.Date(Date), y = Action_Stationary),colour = 'red') + theme_economist_white() + 
  xlab('Date') + ylab('Portfolio Value (in Dollars)') + 
  ggtitle('Stationary vs Adapaptive (A.I.)')


