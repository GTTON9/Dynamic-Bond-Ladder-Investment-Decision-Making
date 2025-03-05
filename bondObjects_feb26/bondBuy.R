bondBuy <- setRefClass("bondBuy",
  fields = list(
    units = "numeric",
    bondObj = "ANY",
    name = "character",
    date = "Date"
  ),
  methods = list(
    initialize = function(units, bondObj, date) {
      .self$units <<- units
      .self$bondObj <<- bondObj
      .self$name <<- "buy"
      .self$date <<- as.Date(date)
    },
    
    getUnits = function() {
      return(.self$units)
    },
    
    setUnits = function(newUnits) {
      .self$units <- newUnits
    },
    
    getBondObj = function() {
      return(.self$bondObj)
    },
    
    setBondObj = function(newObj) {
      .self$bondObj <- newObj
    },
    
    getBondDate = function() {
      return(.self$date)
    },
    
    setBondDate = function(newDate) {
      .self$date <- as.Date(newDate)
    },
    
    getName = function() {
      return(.self$name)
    },
    
    setName = function(newName) {
      .self$name <- newName
    }
  )
)


# Define the bondBuy class (already provided in your code)

# Example usage:

# Assume bondObj is a placeholder bond object
# bond <- bondType(couponRate = 0.05, 
#                      issueDate = "2023-01-01", 
#                      maturityDate = "2033-01-01", 
#                      period = 6, 
#                      countingConvention = "30/360", 
#                      callable = TRUE)
# 
# buyAction <- bondBuy(units = 5,
#                      bondObj = bond,
#                      date = '2025-01-01'
#                      )
# # Accessing attributes using getter methods
# print(paste("Units Bought:", buyAction$getUnits()))                  # Output: 1000
# buyAction$getBondObj()# Output: "US Treasury"
# print(paste("Bond Purchase Date:", buyAction$getBondDate()))         # Output: "2023-06-15"
# print(paste("Action Name:", buyAction$getName()))                    # Output: "buy"
# 
# # Updating attributes using setter methods
# buyAction$setUnits(1500)
# buyAction$setBondDate("2023-07-01")
# buyAction$setName("purchase") # this shouldn't be applicable
# 
# # Verifying the updated attributes
# print(paste("Updated Units Bought:", buyAction$getUnits()))          # Output: 1500
# print(paste("Updated Bond Purchase Date:", buyAction$getBondDate())) # Output: "2023-07-01"
# print(paste("Updated Action Name:", buyAction$getName()))            # Output: "purchase"


