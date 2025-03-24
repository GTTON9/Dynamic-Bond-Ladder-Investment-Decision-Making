source("aveBondSrc/monthsBtw.R")

findPaths <- setRefClass("findPaths",
  fields = list(
    portObj = 'ANY',
    endDate = 'character'
  ),
  
  methods = list(
    initialize = function(portObj = NULL, endDate = NA_character_){
      .self$portObj <- portObj
      .self$endDate <- endDate
    },
    
    getPortObj = function(){
      return(.self$portObj)
    },
    
    setPorObj = function(newPort){
      .self$portObj <- newPort
    },
    
    getEndDate = function(){
      return(.self$endDate)
    },
    
    setEndDate = function(newDate){
      .self$endDate <- newDate
    },
    
    walkPaths = function(){
      bDate <- .self$portObj$getCurrDate()
      eDate <- .self$getEndDate()
      bpSched <- seq(as.Date(bDate),as.Date(eDate),1)
      invSched <- as.Date(monthsBtw(as.Date(bDate),as.Date(eDate))) - 1
      
      portDF <- as.data.frame(matrix(nrow = length(bpSched),ncol = 2))
      colnames(portDF) <- c('Date','portVal')
      
      nadaDF <- as.data.frame(matrix(nrow = length(bpSched),ncol = 2))
      colnames(nadaDF) <- c('Date','portVal')
      
      nadaPort <- .self$portObj$copy()
      
      idCount <- 3
      
      for(i in 1:length(bpSched)){
        print('working')
        
        if(bpSched[i] != as.Date(.self$portObj$getCurrDate())){
          stop('Houston we have a problem')
        }
        
        currTermin <- invSched[invSched >= bpSched[i]][1]
        print(currTermin)
        addCond <- (as.numeric(currTermin - bpSched[i]) == 0)
        currIDs <- .self$portObj$bondLedger$getActTable()$bondID
        
        if(addCond){
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
          
          # Fix this later by making bonds only purchaseable on trading days you know. For now I use "mean" function.
          currVal <- mean(.self$portObj$getPortVal())
          portDF[i,] <- c(as.character(bpSched[i]),currVal)
          nadaVal <- mean(nadaPort$getPortVal())
          nadaDF[i,] <- c(as.character(bpSched[i]),nadaVal)
          .self$portObj$bondUpdate('buy',numUnits = 5000, bondType = newBond, moveForward = TRUE, notChecked = TRUE)
          nadaPort$bondUpdate('buy',numUnits = 5000, bondType = newBond, moveForward = TRUE, notChecked = TRUE)
          
          nadaPort$yieldObj$appRealYields(futureYields[i,],as.character(.self$portObj$getCurrDate()))
          .self$portObj$yieldObj$appRealYields(futureYields[i,],.self$portObj$getCurrDate())
          .self$portObj$yieldObj$computeOutKF()
          
          next
        }
        
        # Use length on currIDs
        if(!is.null(currIDs) & !is.logical(currIDs) & length(currIDs) != 0){
          
          prevW <- -Inf
          maxResult <- NA
          
          for(j in currIDs){
            #
            result <- vRecur(.self$portObj,currDate = .self$portObj$getCurrDate(),
                             terminalDate = as.character(currTermin),j)
            W <- result$W
            if(W > prevW){
              maxResult <- result
            }
            prevW <- W
          
          }
          action <- maxResult$bestAction
          bondType <- maxResult$bondType
          resDate <- maxResult$currDate
          
          if(!is.na(action) & (.self$portObj$getCurrDate() == resDate)){
            .self$portObj$bondUpdate(action, bondType = bondType, moveForward = FALSE, notChecked = TRUE)
            print(action)
          }
          
          currVal <- mean(.self$portObj$getPortVal())
          portDF[i,] <- c(as.character(bpSched[i]),currVal)
          
          .self$portObj$bondUpdate('none',moveForward = TRUE)
        }else{
          someVals <- .self$portObj$getPortVal()
          print(length(someVals))
          currVal <- mean(.self$portObj$getPortVal())
          portDF[i,] <- c(as.character(bpSched[i]),currVal)
          .self$portObj$bondUpdate('none',moveForward = TRUE)
        }
        # .self$portObj$yieldObj$outKF[[6]] == bpSched[i]
        nadaPort$setYieldObj(.self$portObj$getYieldObj()$copy())
        nadaVal <- nadaPort$getPortVal()
        nadaDF[i,] <- c(as.character(bpSched[i]),nadaVal)
        nadaPort$bondUpdate('none',moveForward = TRUE)
        
        nadaPort$yieldObj$appRealYields(futureYields[i,],.self$portObj$getCurrDate())
        .self$portObj$yieldObj$appRealYields(futureYields[i,],.self$portObj$getCurrDate())
        .self$portObj$yieldObj$computeOutKF()
        
      }
      
      return(list(nadaDF = nadaDF,portDF = portDF))
    }
    
    
    
  )
)
