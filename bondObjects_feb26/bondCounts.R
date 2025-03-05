# Define the bondCounts class
bondCounts <- setRefClass("bondCounts",
  fields = list(
    bondDF = "list",
    valDate = 'Date',
    pv = 'numeric'
  ),
  
  methods = list(
    initialize = function(startDate) {
      .self$bondDF <<- list(Date = c(),
                            bondItem = c(),
                            unitsTraded = c(),
                            numberStripped = c())
      .self$valDate <<- startDate
      .self$pv <<- 0
    },
    
    moveForward = function(doBondAction) {
      
    }
      
      
    bondAction = function(doBondAction) {
      bondActionKey <- c("buy" = 1, "sell" = -1, "strip" = 0)
      
      .self$bondDF$Date <- c(.self$bondDF$Date, doBondAction$getBondDate())
      .self$bondDF$bondItem <- c(.self$bondDF$bondItem, doBondAction$getBondObj())
      .self$bondDF$unitsTraded <- c(.self$bondDF$unitsTraded, 
                                          doBondAction$getUnits() * bondActionKey[doBondAction$getName()])
      .self$bondDF$numberStripped <- c(.self$bondDF$numberStripped,
                                       ifelse(doBondAction$getName() == "strip", doBondAction$getUnits(), 0))
    },
    
    getBondDF = function() {
      return(bondDF)
    },
    
    setBondDF = function(newBondDF) {
      .self$bondDF <- newBondDF
    }
  )
)

# Example Usage


# bond1 <- bondType(0.03, '2023-03-06', '2024-03-06', 1, '30/365', TRUE)
# bond2 <- bondType(0.04, '2023-01-01', '2033-01-01', 10, '30/360', FALSE)
# 
# bi1 <- bondItem(bond1, TRUE)
# bi2 <- bondItem(bond2, TRUE)
# 
# act1 <- bondBuy(5, bi1, '2025-01-01')
# act2 <- bondSell(3, bi2, '2025-01-01')
# act3 <- bondSell(2, bi1, '2025-01-03')
# act4 <- bondStrip(1, bi2, '2026-01-03')
# 
# bc <- bondCounts()
# 
# bc$bondAction(act1)
# bc$bondAction(act2)
# bc$bondAction(act3)
# bc$bondAction(act4)
# 
# bc



