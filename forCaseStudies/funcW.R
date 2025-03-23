funcW <- function(bp,currDate = NA,terminalDate = NA,bondID = NA){
  
  currActTable <- bp$getBondLedgerOb()$getActTable()
  coupBools <- currActTable[currActTable[,1] == bondID,2]
  bondDict <- bp$getBondLedgerOb()$getBondDict()
  
  
  actSpace <- c('sell','strip','none')
  valVec <- c(NA,NA,NA)
  preVal <- -Inf
  currInd <- NA
  
  forMat <- 1:length(actSpace)
  for(i in forMat){

      if(actSpace[i] == 'strip' & coupBools == FALSE){
        next
      }
      
      currBP <- bp$copy()
      currBP$bondUpdate(actSpace[i],numUnits = NA, bondType = bondDict[[bondID]], 
                        moveForward = FALSE, notChecked = TRUE)
      
      while(as.Date(currBP$getCurrDate()) < as.Date(terminalDate)){
        currBP$bondUpdate('none')
        
      }

      currVal <- mean(currBP$getPortVal())

      
      valVec[i] <- currVal
      if(currVal > preVal){
        preVal <- currVal

        currInd <- i
      }
      
    
  }

  bestAction <- actSpace[currInd]
  
  if(bestAction != 'none'){
    bestBond <- bondDict[[bondID]]
  }else{
    bestBond <- NA
  }
  
  return(list(bestAction = bestAction, bondType = bestBond, W = preVal - valVec[3],
              currDate = currDate))
}
