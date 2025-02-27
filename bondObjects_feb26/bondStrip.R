# Define the bondStrip class
bondStrip <- setRefClass("bondStrip",
   fields = list(
     units = "numeric",
     bondObj = "ANY",  # Stores a bond object, validated manually
     name = "character",
     date = "Date"  # Stores the date as a Date object
   ),
   
   methods = list(
     initialize = function(units, bondObj, date) {
       .self$units <<- units
       .self$bondObj <<- bondObj
       .self$name <<- "strip"
       .self$date <<- as.Date(date)  # Convert input date to Date object
       bondObj$hasCoupon <<- FALSE
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
       .self$date <- as.Date(newDate)  # Ensure date is stored properly
     },
     
     getName = function() {
       return(.self$name)
     },
     
     setName = function(newName) {
       .self$name <- newName
     }
   )
)


# Create a simple bond object (could be another reference class instance)
bond <- bondType(couponRate = 0.05, 
                     issueDate = "2023-01-01", 
                     maturityDate = "2033-01-01", 
                     period = 6, 
                     countingConvention = "30/360", 
                     callable = TRUE)
# Example bond object

# Create a bondStrip object
stripAction <- bondStrip$new(units = 500, bondObj = bond, date = "2023-07-01")

# Accessing attributes using getter methods
print(paste("Units Stripped:", stripAction$getUnits()))               # Output: 500
stripAction$getBondObj()   
print(paste("Bond Strip Date:", stripAction$getBondDate()))           # Output: "2023-07-01"
print(paste("Action Name:", stripAction$getName()))                   # Output: "strip"

# Updating attributes using setter methods
stripAction$setUnits(700)
stripAction$setBondDate("2023-08-01")
stripAction$setName("strip-action")

# Verifying the updated attributes
print(paste("Updated Units Stripped:", stripAction$getUnits()))        # Output: 700
print(paste("Updated Bond Strip Date:", stripAction$getBondDate()))   # Output: "2023-08-01"
print(paste("Updated Action Name:", stripAction$getName()))           # Output: "strip-action"

