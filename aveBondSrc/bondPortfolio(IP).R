source('aveBondSrc/countConvention.R')
source('aveBondSrc/bondLedger.R')

bondPortfolio <- setRefClass("bondPortfolio",
  fields = list(
    bondLedger = 'ANY',
    cashPosition = "ANY",
    currDate = 'character',
    posList = "list"
  ),
  
  methods = list(
    initialize = function(bondLedger = NULL,cashPosition = NULL,currDate = NA_character_) {
      
      .self$posList <- list()
      .self$posList[[1]] <- bondLedger
      .self$posList[[2]] <- cashPosition
      
      .self$currDate <- currDate
      
      # Use `bondDict <- setNames(bondDict,nameVec)` for setting indexing. 
    },
    
    
    getBondLedgerOb = function(){
      return(.self$postList[[1]])
    },
    
    setBondLedgerOb = function(newBondLedgerOb){
      .self$posList[[1]] <- newBondLedgerOb
    },
    
    getCashPosOb = function(){
      return(.self$postList[[2]])
    },
    
    setCashPosOb = function(newCashPosOb){
      .self$posList[[2]] <- newCashPosOb
    },
    
    getCurrDate = function(){
      return(.self$currDate)
    },
    
    setCurrDate = function(newDate){
      .self$currDate <- newCurrDate
    },
    
    
    getCashPos = function(){
      return(.self$getCashPosOb()$getPos())
    },
    
    
    # Let's say you already created a new actTable as part of entertaining possible action.
    
    .getAssetPV = function(valDate, yieldMC,realYields, bondType, numUnits, hasCoupon) {
      

      if (as.Date(valDate) < as.Date(bondType$issueDate)) {
        stop('The Valuation Date is before the issue Date!')
      }

      
      if(hasCoupon){
        
        # Find the time to maturity of the bond
        ttm <- timeToMaturity(as.Date(valDate),
                              as.Date(bondType$maturityDate),
                              bondType$countingConvention)
        
        pvFace <- 1/(1+yield) ** ttm
      
        # Convert the period to months 
        sched <- as.Date(bondType$bondSchedule) # This will store the schedule of coupons
        

        sched <- as.Date(sched)
        # compute times to maturity
        ttmCoupons <- sapply(X = sched,
                             FUN = timeToMaturity,
                             trading_day = as.Date(valDate),
                             convention = bondType$countingConvention)
        
        
        
        # Now, we compute NS basis functions in the form of a matrix
        # number of cash flows left:
        
        
        yields <- yieldMC(valDate,unique(c(ttmCoupons[ttmCoupons > 0],ttm)),realYields) / 100 # yields to maturity  
        pvCoupons <- (bondType$couponRate / period) * (1 + yields[,1:(ncol(yields)-1)]) ** 
          (-ttmCoupons[ttmCoupons > 0])
        pvCoupons <- as.vector(rowSums(pvCoupons))

        
        yield <- yieldMC(ttm) / 100
        
        pvFace <- as.vector(1/(1+yields[,ncol(yields)]) ** ttm)
      }
        
      else{
        # Find the time to maturity of the bond
        ttm <- timeToMaturity(as.Date(valDate),
                              as.Date(bondType$maturityDate),
                              bondType$countingConvention)
        yields <- yieldMC(valDate,ttm,realYields) / 100
        pvFace <- as.vector(1/(1+yields) ** ttm)
        pvCoupons <- 0
      }
      
      thePV <- pvCoupons + pvFace
      return(numUnits * thePV)
    },
    
    getAssetPV = function(valDate, yieldMC, actTable, bondDict){
      
      assetPV <- 0
      for(i in 1:nrow(actTable)){
        currOb <- actTable[i,]
        currPV <- .self$.getAssetPV(valDate = valDate, yieldMC = yieldMC, bondType = bondDict[[currOb[1]]], 
               numUnits = currOb[[2]], hasCoupon = currOb[[3]])
        assetPV <- assetPV + currPV
      }
      return(assetPV)
    },
    
    
    bondUpdate = function(action,numUnits = NA, bondType = NA, moveForward = TRUE, notChecked = TRUE){
      
      currActTable <- .self$getBondLedgerOb()$actTable
      currTranTable <- .self$getBondLedgerOb()$tranTable
      currBondDict <- .self$getBondLedgerOb()$bondDict
      currCashPos <- .self$getCashPosOb()
      
      noAdj <- TRUE
      
      # Bring in predetermined cashflows and initialize.
      
      if(moveForward & notChecked){
        newDate <- as.character(as.Date(.self$currDate) + 1)
        idList <- currActTable$bondID

        for(a in idList){
          currBondType <- currBondDict[[a]]
          currSched <- if(currBondType$bondSchedule != '') as.Date(currBondType$bondSchedule) else
            ''
          
          if(newDate %in% currSched){
            cpn_rate <- currBondType$getCouponRate() / currBondType$getPeriod()
            cpn <- currActTable[currActTable$bondID == a,"numUnits"] * cpn_rate
            currCashPos$updateCashPos('cashUp',cpn,noAdj)
            
            transOb <- c(as.character(newDate),cpn,'Receive Coupon',a)
            currTranTable <- rbind(currTranTable,transOb)
            noAdj <- FALSE
          }
          
          matDate <- currBondType$getMaturityDate()
          if(newDate == as.Date(matDate)){
            FV <- 1 * currActTable[currActTable$bondID == a,"numUnits"]
            currCashPos$updateCashPos('cashUp',FV,noAdj)
            
            transOb <- c(as.character(newDate), FV, 'Receive Redemption Amount', a)
            currActTable <- currActTable[-which(currActTable$bondID == a),]
            
            currTranTable <- rbind(currTranTable, transOb)
            noAdj <- FALSE
          }
        }
        
        currCashPos$setCurrDate(as.character(newDate))
      }
      else{
        newDate <- as.Date(.self$currDate)
      }
      
      # Actions
      
      ## Sell
      

      if(!is.na(bondType)){
        bondID <- bondType$getBondID()
        currBondType <- bondType

        
        if(action == 'sell'){
          
          if(is.na(numUnits)){
            
            currBond <- currActTable[which(currActTable$bondID == bondID),]
            currPV <- .getAssetPV(newDate, yieldMC, currBondType, currBond[3], currBond[2])
            currCashPos$updateCashPos('cashUp',currPV,noAdj)
            
            transOb <- c(as.character(newDate), currPV, 'Sell All', bondID)
            currTranTable <- rbind(currTranTable, transOb)
            
            
            currActTable <- currActTable[-which(currActTable$bondID == bondID),]
            
          }
          else{
            stop('Sell for x out of n number of units not implemented.')
          }
          
        }
        
        else if(action == 'buy'){
          
          if(is.na(numUnits)){
            

            
            currBond <- c(bondID, len(bondType$getBondSchedule()) != 0, numUnits)
            currPV <- .getAssetPV(newDate, yieldMC, currBondType, currBond[3], currBond[2])
            currCashPos$updateCashPos('cashDown',currPV,noAdj)
            
            transOb <- c(as.character(newDate), currPV, 'Buy', bondID)
            currTranTable <- rbind(currTranTable, transOb)
            
            
            currActTable <- rbind(currActTable,currBond)
            
            if(!(bondID %in% names(currBondDict))){
              currBondDict[[bondID]] <- currBondType
            }
            
          }
          else{
            stop('Sell for x out of n number of units not implemented.')
          }
          
        }
        
        else if(action == 'strip'){
          
          if(is.na(numUnits)){
            

            
            currBond <- currActTable[which(currActTable$bondID == bondID),]
            
            if(currBond[2] == FALSE){
              stop("Bond is already stripped.")
            }
            

            currPV <- .getAssetPV(newDate, yieldMC, currBondType, currBond[3], currBond[2])
            currBond[2] <- FALSE
            currFV <- .getAssetPV(newDate, yieldMC, currBondType, currBond[3], currBond[2])
            theDiff <- currPV - currFV
            
            currCashPos$updateCashPos('cashUp',theDiff,noAdj)
            currActTable[which(currActTable$bondID == bondID),] <- currBond
            
            transOb <- c(as.character(newDate), theDiff, 'Strip All', bondID)
            currTranTable <- rbind(currTranTable, transOb)
          }
          else{
            stop('Sell for x out of n number of units not implemented.')
          }
          
        }
        else if(action == 'none'){
          currCashPos$updateCashPos('none',0,noAdj)
        }
        
      }
      
      # Wrap up adjustments.
      
      newBondLedger <- bondLedger(currDate = as.character(newDate))
      newBondLedger$setTranTable(currTranTable)
      newBondLedger$setActTable(currActTable)
      newBondLedger$setBondDict(currBondDict)
      
      .self$setBondLedgerOb(newBondLedger)
      .self$setCurrDate(as.character(newDate))
      .self$setCashPosOb(currCashPos)
      
    }
  )
)
