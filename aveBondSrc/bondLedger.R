bondLedger <- setRefClass("bondLedger",
  fields = list(
    tranTable = 'data.frame',
    actTable = 'data.frame',
    bondDict = 'list',
    currDate = 'character'
  ),
  
  methods = list(
    initialize = function(currDate = NA_character_) {
      .self$tranTable <- data.frame(matrix(ncol = 4,nrow = 0))
      colnames(.self$tranTable) <- c('transDate','amt','action','bondID')
      
      .self$actTable <- data.frame(matrix(ncol = 3,nrow = 0))
      colnames(.self$actTable) <- c("bondID",'hasCoupons',"numUnits")
      
      .self$currDate <- currDate
      .self$bondDict <- list()
      
      # Use `bondDict <- setNames(bondDict,nameVec)` for setting indexing. 
    },
    
    
    getTranTable = function() {
      return(.self$tranTable)
    },
    
    setTranTable = function(newTable) {
      .self$tranTable <- newTable
    },
    
    getActTable = function() {
      return(.self$actTable)
    },
    
    setActTable = function(newTable) {
      .self$actTable <- newTable
    },
    
    getCurrDate = function() {
      return(.self$currDate)
    },
    
    setCurrDate = function(newDate) {
      .self$currDate <- newDate
    },
    
    getBondDict = function() {
      return(.self$bondDict)
    },
    
    setBondDict = function(newDict) {
      .self$bondDict <- newDict
    },
    
    
    
    appTranTable = function(newObs){
      tempNames <- names(.self$tranTable)
      .self$tranTable <- rbind(.self$tranTable,newObs)
      names(.self$tranTable) <- tempNames
    },
    
    rmTranObs = function(idList){
      ids <- which(.self$tranTable$bondID %in% idList)
      .self$tranTable <- .self$tranTable[-ids,]
    },
    
    appActTable = function(newObs){
      tempNames <- names(.self$actTable)
      .self$actTable <- rbind(.self$actTable,newObs)
      names(.self$actTable) <- tempNames
    },
    
    rmActObs = function(idList){
      ids <- which(.self$actTable$bondID %in% idList)
      .self$actTable <- .self$actTable[-ids,]
    },
    
    appendBondDict = function(newBond){
      eval(parse(text=paste(".self$bondDict$", 
              paste('b',as.character(length(.self$bondDict) + 1),sep = "")
              ,' <- ',"newBond", sep = "")))
    },
    
    getDictVal = function(key){
      eval(parse(text=paste('return(',".self$bondDict$",key,')',sep = "")))
    }
    
  )
)

# bL <- bondLedger('2024-01-01')
# bL$appendBondDict(data.frame(x = 1:5,y = 6:10))
# bL$appendBondDict(c(5,10))
# bL$getBondDict()
# bL$getDictVal('b2')
