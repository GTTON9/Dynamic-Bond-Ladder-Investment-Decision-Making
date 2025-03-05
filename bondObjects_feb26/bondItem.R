source('countConvention.R')
library(ggplot2)
b1 <- bondItem(bType, T) 

b1$getPV(c(5, 0, 0), .5, '2023-1-1')

bondItem <- setRefClass("bondItem",
  fields = list(
    bondType = "ANY",
    hasCoupon = "logical",
    couponRate = "numeric",
    issueDate = "character",        # Assuming date stored as a string
    maturityDate = "character",      # Can convert to Date if needed
    period = 'numeric',
    countingConvention = "character",
    callable = "logical"
  ),
  methods = list(
    initialize = function(bondType, hasCoupon) {
      .self$bondType <<- bondType # All relevant attributes of the bond
      .self$hasCoupon <<- hasCoupon # This is mutable 
      .self$couponRate <<- bondType$getCouponRate()
      .self$issueDate <<- bondType$getIssueDate()
      .self$maturityDate <<- bondType$getMaturityDate()
      .self$period <<- bondType$getPeriod()
      .self$countingConvention <<- bondType$getCountingConvention()
      .self$callable <<- bondType$getCallable()
    },
    getPV = function(state, lambda, valDate) {
      # Takes in:
      # - state: forecast of the state
      # - lambda: decay parameter
      # - valDate: date at which we want to evaluate the present value of the bond
      
      # First, We must figure out the coupon schedule
      # If the bond has no coupons, we only care of the PV of the face value
      if (as.Date(valDate) < as.Date(.self$issueDate)) {
        stop('The Valuation Date is before the issue Date!')
      }
      if (!.self$hasCoupon) {
        
        # Find the time to maturity of the bond
        ttm <- timeToMaturity(as.Date(valDate),
                              as.Date(.self$maturityDate),
                              .self$countingConvention)
        nsMat <- t(c(1,
                     (1-exp(-lambda * ttm))/(lambda * ttm),
                     (1-exp(-lambda * ttm))/(lambda * ttm) - exp(- lambda * ttm)))
        
        yield <- nsMat %*% state / 100
        
        pvFace <- 1/(1+yield) ** ttm
        
        return(pvFace)
      }
      
      # Convert the period to months 
      h <- period * 12
      sched <- c() # This will store the schedule of coupons
      i <- 1 
      repeat {
        # The maturity dates for coupons are calculated here: 
        nextCoupDate <- as.Date(add_months(as.Date(.self$issueDate), h * i)) # adds h months 
        
        # Ends loop when we go past maturity
        
        if (as.Date(nextCoupDate) >= as.Date(.self$maturityDate)) {
          break
        } 
        
        # append next coupon maturity date only if ttm is non-negative
        if (as.Date(valDate) <= nextCoupDate)
          sched <- c(sched, nextCoupDate) 
        i <- i + 1
      }
      if (length(sched) > 0) {
        sched <- as.Date(sched)
        # compute times to maturity
        ttmCoupons <- sapply(X = sched,
                             FUN = timeToMaturity,
                             trading_day = as.Date(valDate),
                             convention = .self$countingConvention)
        
        # change values of 0 to avoid numerical problems:
        ttmCoupons[which(ttmCoupons == 0)] <- 0.000001
      } else {
        ttmCoupons <- c()
      }

      ttmFace <- timeToMaturity(as.Date(valDate),
                                as.Date(.self$maturityDate),
                                .self$countingConvention)

      if (ttmFace == 0) {
        ttmFace <- 0.000001  
      }
      ttm <- c(ttmCoupons, ttmFace)

      # Now, we compute NS basis functions in the form of a matrix
      # number of cash flows left:
      numCFLeft <- length(ttm) # number of coupons left  
      nsMat <- cbind(rep(1,numCFLeft), # Level
                     (1-exp(-lambda * ttm))/(lambda * ttm), # Slope
                     (1-exp(-lambda * ttm))/(lambda * ttm) - exp(-lambda * ttm)) # Curv
      yields <- nsMat %*% state / 100# yields to maturity  
      pvCoupons <- .self$couponRate * period / (1 + yields[-numCFLeft]) ** (ttmCoupons)
      pvAllCoupons <- sum(pvCoupons)
      pvFace <- 1 / (1 + yields[numCFLeft]) ** ttmFace

      return(pvCoupons + pvFace)
    }
  )
)  

# bType <- bondType(
#   couponRate = 0.05,
#   issueDate = '2023-01-01',
#   maturityDate = '2033-01-01',
#   period = 1/2,
#   countingConvention = '30/360',
#   callable = T
# )
# 
# stripBond1 <- bondStrip(1, bond1, '2023-02-01')

