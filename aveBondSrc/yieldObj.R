# source("yieldsMC.R")
source("aveBondSrc/phoronYield.R")
source("aveBondSrc/bootstrapFuncs.R")

dupInd <- function(someVec){
  
  if(!(TRUE %in% duplicated(someVec))){
    ordVec <- someVec[order(someVec)]
    return(match(someVec,ordVec))
  }
  
  ordInd <- order(someVec)
  ordVec <- someVec[ordInd]
  
  lwr <- anyDuplicated(ordVec,fromLast = T)
  upr <- anyDuplicated(ordVec,fromLast = F)
  dupVal <- ordVec[upr]
  
  
  
  
  ordInd[which(ordInd == lwr)] <- upr
  
  ordInd[ordVec >= dupVal] <- ordInd[ordVec >= dupVal] - 1
  
  return(ordInd)
}


yieldObj <- setRefClass("yieldObj",
  fields = list(
    realYields = "matrix",
    yieldTenors = "numeric",
    outKF = "list"
  ),
  
  methods = list(
    initialize = function(realYields = NULL, outKF = list(), yieldTenors = NA_real_
    ) {
      .self$realYields <- realYields
      .self$outKF <- outKF
      .self$yieldTenors <- yieldTenors
    },
    
    # Get and Set functions
    
    getRealYields = function(){
      return(.self$realYields)
    },
    
    setRealYields = function(newYields){
      .self$realYields = newYields
    },
    
    getOutKF = function(){
      return(.self$outKF)
    },
    
    setOutKF = function(newOut){
      .self$outKF = newOut
    },
    
    getYieldTenors = function(){
      return(.self$yieldTenors)
    },
    
    setYieldTenors = function(newTenors){
      .self$YieldTenors = newTenors
    },
    
    
    # Main functions
    
    computeOutKF = function(){
      outPut <- .yieldMC(.self$realYields)
      .self$setOutKF(outPut)
    },
    
    simTen = function(valDate,tenors,simCount = 100){
      

      
      outPut <- .self$getOutKF()

      hStep <- as.numeric(as.Date(valDate) - as.Date(outPut[[6]]))
      

      dupInds <- dupInd(tenors)
      if(hStep <= 0){
        stop("Not forecasting outside of sample.")
      }
      
      someTens <- outPut[[8]]
      C <- outPut[[7]]
      A <- outPut[[1]]
      
      interFunc <- function(h){
        return( do.call(cbind,lapply((h - 1):0,FUN = function(h) A %^% h )) ) 
      }
      
      u <- outPut[[9]]
      bigTerm <- interFunc(hStep) %*% rep(u,hStep)
      
      
      foreEX <- C %*% ((A %^% hStep) %*% outPut[[2]]  + bigTerm)
      
      foreVAR <-  (A %^% hStep) %*% outPut[[3]] %*% t(A %^% hStep) + outPut[[5]]
      if(hStep > 1){
        for(i in 1:(hStep - 1)){
          foreVAR <- foreVAR + (A %^% i) %*% outPut[[5]] %*% t((A %^% i))
        }
      }
      
      foreVAR <- C %*% foreVAR %*% t(C) + outPut[[4]]
      

      tempVar <-mvrnorm(n = simCount,mu = foreEX,Sigma = foreVAR)
      
      simMat <- interp_yc(someTens,tempVar,unique(sort(tenors)))
      
      
      return(simMat)
    },
    
    appRealYields = function(newObs){
      tempNames <- names(.self$realYields)
      tempObs <- newObs
      names(tempObs) <- tempNames
      .self$realYields <- rbind(.self$realYields, tempObs)
      names(.self$realYields) <- tempNames
    },
    
    yieldInS = function(valDate,tenors){
      
      lastDate = rownames(.self$getRealYields())
      lastDate = lastDate[length(lastDate)]
      
      if(as.Date(lastDate) < valDate){
        stop("Attempting to find yields beyond bond sample information. Use simTen instead.")
      }
      
      currBondYields <- .self$realYields[as.character(valDate),]
      currTTM <- .self$getYieldTenors()
      dupInds <- dupInd(tenors)
      
      return(interp_yc(currTTM,currBondYields,unique(sort(tenors)))[dupInds])
      
    }
  )
)