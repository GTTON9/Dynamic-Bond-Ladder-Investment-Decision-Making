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
      begDate <- .self$portObj$getCurrDate()
      endDate <- .self$getEndDate()
      bpSched <- seq(as.Date(begDate),as.Date(endDate),1)
      invSched <- as.Date(monthsBtw(as.Date(begDate),as.Date(endDate)))
      
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
        
        currTermin <- invSched[invSched > bpSched[i]][1]
        newPor <- .self$portObj$copy()
        currIDs <- .self$portObj$bondLedger$getActTable()$bondID
        
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
          
          currVal <- mean(.self$portObj$getPortVal())
          portDF[i,] <- c(as.character(bpSched[i]),currVal)
          nadaVal <- nadaPort$getPortVal()
          nadaDF[i,] <- c(as.character(bpSched[i]),nadaVal)
          .self$portObj$bondUpdate('buy',numUnits = 10, bondType = newBond, moveForward = TRUE, notChecked = TRUE)
          nadaPort$bondUpdate('buy',numUnits = 10, bondType = newBond, moveForward = TRUE, notChecked = TRUE)
          
          next
        }
        
        
        if(!is.null(currIDs)){
          
          for(j in currIDs){
            result <- vRecur(.self$portObj,currDate = .self$portObj$getCurrDate(),terminalDate = as.character(currTermin),j)
            action <- result$bestAction
            bondType <- result$bondType
            resDate <- result$currDate
            
            if(!is.na(action) & (.self$portObj$getCurrDate() == resDate)){
              .self$portObj$bondUpdate(action, bondType = bondType, moveForward = FALSE, notChecked = TRUE)
              break
            }
          }
          currVal <- mean(.self$portObj$getPortVal())
          portDF[i,] <- c(as.character(bpSched[i]),currVal)
          
          .self$portObj$bondUpdate('none',moveForward = TRUE)
        }else{
          .self$portObj$bondUpdate('none',moveForward = TRUE)
        }
        
        
        nadaVal <- nadaPort$getPortVal()
        nadaDF[i,] <- c(as.character(bpSched[i]),nadaVal)
        nadaPort$bondUpdate('none',moveForward = TRUE)
        
        nadaPort$yieldObj$appRealYields(futureYields[i,])
        .self$portObj$yieldObj$appRealYields(futureYields[i,])
        .self$portObj$yieldObj$computeOutKF()
        
      }
      
      return(list(nadaDF = nadaDF,portDF = portDF))
    }
    
    
    
  )
)
