source('aveBondSrc/countConvention.R')
source('aveBondSrc/bondLedger.R')

bondPortfolio <- setRefClass("bondPortfolio",
  fields = list(
    bondLedger = 'ANY',
    cashPosition = "ANY",
    currDate = 'character',
    posList = "list",
    yieldObj = "ANY"
  ),
  
  methods = list(
    initialize = function(bondLedger = NULL,cashPosition = NULL,currDate = NA_character_, 
                          yieldObj = NULL) {
      
      .self$posList <- list()
      .self$posList[[1]] <- bondLedger
      .self$posList[[2]] <- cashPosition
      
      .self$currDate <- currDate
      .self$yieldObj <- yieldObj
      
      # Use `bondDict <- setNames(bondDict,nameVec)` for setting indexing. 
    },
    
    
    getBondLedgerOb = function(){
      return(.self$posList[[1]])
    },
    
    setBondLedgerOb = function(newBondLedgerOb){
      .self$posList[[1]] <- newBondLedgerOb
    },
    
    getCashPosOb = function(){
      return(.self$posList[[2]])
    },
    
    setCashPosOb = function(newCashPosOb){
      .self$posList[[2]] <- newCashPosOb
    },
    
    getCurrDate = function(){
      return(.self$currDate)
    },
    
    setCurrDate = function(newDate){
      .self$currDate <- newDate
    },
    
    
    getCashPos = function(){
      return(.self$getCashPosOb()$getPos())
    },
    
    getYieldObj = function(){
      return(.self$yieldObj)
    },
    
    setYieldObj = function(newYieldObj){
      .self$yieldObj = newYieldObj
    },
    
    updateYieldObj = function(newYields = NA, compute = T){
      if(!is.na(newYields)){
        .self$yieldObj$appRealYields(newYields)
      }
      if(compute){
        .self$yieldObj$computeOutKF
      }
    },
    
    
    # Let's say you already created a new actTable as part of entertaining possible action.
    
    .getAssetPV = function(valDate, bondType, numUnits, hasCoupon) {
      
      yieldMC <- function(valDate,tenors){
        return( .self$yieldObj$simTen(valDate,tenors) )
      }
      
      yieldPV <- function(valDate,tenors){
        return(  .self$yieldObj$yieldInS(valDate,tenors))
      }
      
      if (as.Date(valDate) < as.Date(bondType$issueDate)) {
        stop('The Valuation Date is before the issue Date!')
      }

      
      if(hasCoupon){
        period <- bondType$getPeriod()
        
        # Find the time to maturity of the bond
        ttm <- timeToMaturity(as.Date(valDate),
                              as.Date(bondType$maturityDate),
                              bondType$countingConvention)
        
        sched <- as.Date(bondType$bondSchedule) # This will store the schedule of coupons
        


        # compute times to maturity
        ttmCoupons <- sapply(X = sched,
                             FUN = timeToMaturity,
                             trading_day = as.Date(valDate),
                             convention = bondType$countingConvention)
        
        
        
        # Now, we compute NS basis functions in the form of a matrix
        # number of cash flows left:
        
        if(as.Date(valDate) > as.Date(.self$yieldObj$getOutKF()[[6]])){
          
          
          yields <- yieldMC(valDate,c(ttmCoupons[ttmCoupons > 0],ttm)) / 100 # yields to maturity
          bern <- ifelse(ncol(yields) == sum(ttmCoupons > 0),0,1)

          pvCoupons <- (bondType$couponRate / period) * (1 + yields[,1:(ncol(yields)-bern)]) ** 
            (-ttmCoupons[ttmCoupons > 0])
          pvCoupons <- as.vector(rowSums(pvCoupons))
          
          pvFace <- as.vector(1/(1+yields[,ncol(yields)]) ** ttm)
        }
        else{
          yields <- yieldPV(valDate,c(ttmCoupons[ttmCoupons > 0],ttm)) / 100
          
          pvCoupons <- (bondType$getCouponRate() / period) * (1 + yields[-length(yields)]) ** 
            (-ttmCoupons[ttmCoupons > 0])
          pvCoupons <- as.numeric(sum(pvCoupons))
          
          pvFace <- as.numeric(1/(1+yields[length(yields)]) ** ttm)
        }

      }
        
      else{

        if(as.Date(valDate) > as.Date(.self$yieldObj$getOutKF()[[6]])){

          ttm <- timeToMaturity(as.Date(valDate),
                                as.Date(bondType$maturityDate),
                                bondType$countingConvention)
          yields <- yieldMC(valDate,ttm) / 100
          pvFace <- as.vector(1/(1+yields) ** ttm)
          pvCoupons <- 0
        }
        else{
          ttm <- timeToMaturity(as.Date(valDate),
                                as.Date(bondType$maturityDate),
                                bondType$countingConvention)
          yields <- yieldPV(valDate,ttm) / 100
          pvFace <- as.vector(1/(1+yields) ** ttm)
          pvCoupons <- 0
        }
        # Find the time to maturity of the bond

      }
      
      thePV <- pvCoupons + pvFace
      return(numUnits * thePV)
    },
    
    getAssetPV = function(valDate, bondDict){
      
      valDate <- .self$getCurrDate()
      bondDict <- .self$getBondLedgerOb()$getBondDict()
      actTable <- .self$getBondLedgerOb()$getActTable()
      
      assetPV <- 0
      for(i in 1:nrow(actTable)){
        currOb <- actTable[i,]
        currPV <- .self$.getAssetPV(valDate = valDate, bondType = bondDict[[currOb[[1]]]], 
               numUnits = currOb[[3]], hasCoupon = currOb[[2]])
        assetPV <- assetPV + currPV
      }
      return(assetPV)
    },
    
    
    bondUpdate = function(action,numUnits = NA, bondType = NA, moveForward = TRUE, notChecked = TRUE){
      
      currActTable <- .self$getBondLedgerOb()$actTable
      currTranTable <- .self$getBondLedgerOb()$tranTable
      currBondDict <- .self$getBondLedgerOb()$bondDict
      currCashPos <- .self$getCashPosOb()
      
      noAdj <- FALSE
      idList <- currActTable$bondID
      
      # Bring in predetermined cashflows and initialize.
      
      if(moveForward & notChecked & !is.logical(idList)){
        noAdj = TRUE
        newDate <- as.character(as.Date(.self$currDate) + 1)

        
        for(a in idList){
          currBondType <- currBondDict[[a]]
          currSched <- if(length(currBondType$bondSchedule) != 0) as.Date(currBondType$bondSchedule) else
            list()
          
          if(newDate %in% currSched){
            cpn_rate <- currBondType$getCouponRate() / currBondType$getPeriod()
            cpn <- currActTable[currActTable$bondID == a,"numUnits"] * cpn_rate
            currCashPos$updateCashPos('cashUp',cpn,noAdj)
            
            transOb <- list(as.character(newDate),cpn,'Receive Coupon',a)
            tranNames <- names(currTranTable)
            currTranTable <- rbind(currTranTable, transOb)
            names(currTranTable) <- tranNames

            noAdj <- FALSE
          }
          
          matDate <- currBondType$getMaturityDate()
          if(newDate == as.Date(matDate)){
            FV <- 1 * currActTable[currActTable$bondID == a,"numUnits"]
            currCashPos$updateCashPos('cashUp',FV,noAdj)
            
            transOb <- list(as.character(newDate), FV, 'Receive Redemption Amount', a)
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
            currPV <- .getAssetPV(newDate,currBondType, as.numeric(currBond[3]), as.logical(currBond[2]))
            currCashPos$updateCashPos('cashUp',currPV,noAdj)

            transOb <- data.frame(transDate = as.character(newDate), amt = currPV, 
                                  action = 'Sell All', bondID = bondID)
            
            tranNames <- names(currTranTable)
            currTranTable <- rbind(currTranTable, transOb)
            names(currTranTable) <- tranNames

            
            currActTable <- currActTable[-which(currActTable$bondID == bondID),]
            
          }
          else{
            stop('Sell for x out of n number of units not implemented.')
          }
          
        }
        
        else if(action == 'buy'){
          
          if(!is.na(numUnits)){

            currBond <- list(bondID, (length(bondType$getBondSchedule()) != 0), numUnits)

            currPV <- .getAssetPV(newDate, currBondType, currBond[[3]], currBond[[2]])
            currCashPos$updateCashPos('cashDown',currPV,noAdj)
            
            transOb <- data.frame(transDate = as.character(newDate), amt = currPV, 
                                  action = 'Buy', bondID = bondID)
            tranNames <- names(currTranTable)
            currTranTable <- rbind(currTranTable, transOb)
            names(currTranTable) <- tranNames
            
            actNames <- names(currActTable)
            currActTable <- rbind(currActTable,currBond)
            names(currActTable) <- actNames
            
            if(!(bondID %in% names(currBondDict))){
              currBondDict[[bondID]] <- currBondType
            }
            
          }
          else{
            stop('Specify amount of units to purchase.')
          }
          
        }
        
        else if(action == 'strip'){
          
          if(is.na(numUnits)){
            

            
            currBond <- currActTable[which(currActTable$bondID == bondID),]
            
            if(currBond[2] == FALSE){
              stop("Bond is already stripped.")
            }
            

            currPV <- .getAssetPV(newDate, currBondType, as.numeric(currBond[3]), 
                                  as.logical(currBond[2]))
            currBond[2] <- FALSE
            currFV <- .getAssetPV(newDate, currBondType, as.numeric(currBond[3]), 
                                  as.logical(currBond[2]))
            theDiff <- currPV - currFV
            
            currCashPos$updateCashPos('cashUp',theDiff,noAdj)
            currActTable[which(currActTable$bondID == bondID),] <- currBond
            

            transOb <- data.frame(transDate = as.character(newDate), amt = theDiff, 
                                  action = 'Strip All', bondID = bondID)
            tranNames <- names(currTranTable)
            currTranTable <- rbind(currTranTable, transOb)
            names(currTranTable) <- tranNames
          }
          else{
            stop('Strip for x out of n number of units not implemented.')
          }
          
        }
        
      }
      else{
        currCashPos$updateCashPos('none',0,noAdj)
      }
      
      # Wrap up adjustments.
      
      newBondLedger <- bondLedger(currDate = as.character(newDate))
      newBondLedger$setTranTable(currTranTable)
      newBondLedger$setActTable(currActTable)
      newBondLedger$setBondDict(currBondDict)
      currCashPos$setCurrDate(as.character(newDate))
      
      .self$setBondLedgerOb(newBondLedger)
      .self$setCurrDate(as.character(newDate))
      .self$setCashPosOb(currCashPos)
      
    },
    
    getPortVal = function(){
      return( .self$getCashPos() + .self$getAssetPV() )
    }
  )
)
