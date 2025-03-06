source("yieldsMC.R")
source("aveBondSrc/bootstrapFuncs.R")

yieldObj <- setRefClass("yieldObj",
  fields = list(
    realYields = "matrix",
    outKF = "list"
  ),
  
  methods = list(
    initialize = function(realYields = NULL, outKF = list()
    ) {
      .self$realYields <- realYields
      .self$outKF <- outKF
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
    
    computeOutKF = function(){
      outPut <- .yieldMC(.self$realYields)
      .self$setOutKF(outPut)
    },
    
    simTen = function(valDate,tenors,simCount = 1000){
      
      outPut <- .self$getOutKF()
      hStep <- as.numeric(as.Date(valDate) - as.Date(outPut[[6]]))
      
      someTens <- outPut[[8]]
      C <- outPut[[7]]
      A <- outPut[[1]]
      
      foreEX <- C %*% (A %^% hStep) %*% outPut[[2]]
      
      foreVAR <-  (A %^% hStep) %*% outPut[[3]] %*% t(A %^% hStep) + outPut[[5]]
      for(i in 1:(hStep - 1)){
        foreVAR <- foreVAR + (A %^% i) %*% outPut[[5]] %*% t((A %^% i))
      }
      
      foreVAR <- C %*% foreVAR %*% t(C) + outPut[[4]]
      
      simMat <- matrix(nrow = simCount,ncol = length(tenors))
      
      for(i in 1:nrow(simMat)){
        tempVar <- as.vector(foreEX + mvrnorm(mu = rep(0,nrow(foreVAR)), Sigma = foreVAR))
        simMat[i,] <- interp_yc(someTens,tempVar,tenors)$ZERO_YLD1
      }
      
      return(simMat)
    },
    
    appRealYields = function(newObs){
      tempNames <- names(.self$realYields)
      .self$realYields <- rbind(.self$realYields, newObs)
      names(.self$realYields) <- tempNames
    },
    
    somePV = function(something){
      NULL
    }
    
    
    
  )
)