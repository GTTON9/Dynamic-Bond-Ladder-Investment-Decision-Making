source('aveBondSrc/countConvention.R')

bondPortfolio <- setRefClass("bondPortfolio",
  fields = list(
    posList = "list", # Contains cash position object and bondLedger object
    currDate = 'character',
    
    
  ),
  
  methods = list(
    initialize = function(bondLedger = NULL,cashPosition = NULL,currDate = NA_character_) {
      
      .self$posList <<- list()
      .self$posList[[1]] <<- bondLedger
      .self$posList[[2]] <<- cashPosition
      
      .self$currDate <<- currDate
      
      # Use `bondDict <- setNames(bondDict,nameVec)` for setting indexing. 
    },
    
    
    getBondLedgerOb = function(){
      return(.self$postList[[1]])
    },
    
    setBondLedgerOb = function(newBondLedgerOb){
      .self$posList[[1]] <<- newBondLedgerOb
    },
    
    getCashPosOb = function(){
      return(.self$postList[[2]])
    },
    
    setCashPosOb = function(newCashPosOb){
      .self$posList[[2]] <<- newCashPosOb
    },
    
    getCurrDate = function(){
      return(.self$currDate)
    },
    
    setCurrDate = function(newDate){
      .self$currDate <<- newCurrDate
    },
    
    
    getCashPos = function(){
      return(getCashPosOb()$getPos())
    },
    
    
    # Let's say you already created a new actTable as part of entertaining possible action.
    
    .getAssetPV = function(valDate, yieldMC, bondType, numUnits, hasCoupon) {
      

      if (as.Date(valDate) < as.Date(bondType$issueDate)) {
        stop('The Valuation Date is before the issue Date!')
      }

        
      # Find the time to maturity of the bond
      ttm <- timeToMaturity(as.Date(valDate),
                            as.Date(bondType$maturityDate),
                            bondType$countingConvention)
      
      yield <- yieldMC(ttm) / 100
      
      pvFace <- 1/(1+yield) ** ttm
        

      
      if(hasCoupon){
      
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
        
        
        yields <- yieldMC(ttmCoupons) / 100 # yields to maturity  
        pvCoupons <- (bondType$couponRate / period) * (1 + yields) ** (-ttmCoupons[ttmCoupons > 0])
        pvCoupons <- sum(pvCoupons)
      }
        
      else{
        pvCoupons <- 0
      }
      
      thePV <- pvCoupons + pvFace
      return(numUnits * thePV)
    },
    
    getAssetPV <- function(valDate, yieldMC, actTable, bondDict){
      
      assetPV <- 0
      for(i in 1:nrow(actTable)){
        currOb <- actTable[i,]
        currPV <- .getAssetPV(valDate = valDate, yieldMC = yieldMC, bondType = bondDict[[currOb[1]]], 
               numUnits = currOb[[2]], hasCoupon = currOb[[3]])
        assetPV <- assetPV + currPV
      }
      return(assetPV)
    },
    
    bondUpdate <- function(action)
  )
)
