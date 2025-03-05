# Define the bondType class
bondType <- setRefClass("bondType",
  fields = list(
    couponRate = "numeric",
    issueDate = "character",        # Assuming date stored as a string
    maturityDate = "character",      # Can convert to Date if needed
    period = "numeric",
    countingConvention = "character",
    callable = "logical",
    bondID = "numeric",
    bondSchedule = "ANY"
  ),
  
  methods = list(
    initialize = function(couponRate = NA_real_, issueDate = NA_character_, 
                          maturityDate = NA_character_, period = NA_real_, 
                          countingConvention = NA_character_, 
                          bondID = NA_real_,bondSchedule = NULL,callable = FALSE) {
      .self$couponRate <- couponRate
      .self$issueDate <- issueDate
      .self$maturityDate <- maturityDate
      .self$period <- period
      .self$countingConvention <- countingConvention
      .self$bondID <- bondID
      .self$bondSchedule <- bondSchedule
      .self$callable <- callable
    },
    
    getCouponRate = function() {
      return(.self$couponRate)
    },
    
    setCouponRate = function(newRate) {
      .self$couponRate <- newRate
    },
    
    getIssueDate = function() {
      return(.self$issueDate)
    },
    
    setIssueDate = function(newDate) {
      .self$issueDate <- newDate
    },
    
    getMaturityDate = function() {
      return(.self$maturityDate)
    },
    
    setMaturityDate = function(newDate) {
      .self$maturityDate <- newDate
    },
    
    getCountingConvention = function() {
      return(.self$countingConvention)
    },
    
    setCountingConvention = function(newConvention) {
      .self$countingConvention <- newConvention
    },
    
    getPeriod = function() {
      return(.self$period)
    },
    
    setPeriod = function(newPeriod) {
      .self$period <- newPeriod 
    },
    
    getBondID = function() {
      return(.self$bondID)
    },
    
    setBondID = function(newID) {
      .self$bondID <- newID 
    },
    
    getBondSchedule = function() {
      return(.self$bondSchedule)
    },
    
    setBondSchedule = function(newSchedule) {
      .self$bondSchedule <- newSchedule 
    },
    
    getCallable = function() {
      return(.self$callable)
    },
    
    setCallable = function(newCallable) {
      .self$callable <- newCallable
    }
  )
)

# Define the bondType class (same as above)

# # Create a new bondType object
bond <- bondType(couponRate = 0.05,
                     issueDate = "2023-01-01",
                     maturityDate = "2033-01-01",
                     period = 6,
                     countingConvention = "30/360",
                     bondID = 0,
                     bondSchedule = list(''),
                     callable = TRUE)

# # Accessing attributes using getter methods
# print(paste("Coupon Rate:", bond$getCouponRate()))            # Output: 0.05
# print(paste("Issue Date:", bond$getIssueDate()))              # Output: "2023-01-01"
# print(paste("Maturity Date:", bond$getMaturityDate()))        # Output: "2033-01-01"
# print(paste("Counting Convention:", bond$getCountingConvention()))  # Output: "30/360"
# print(paste("Is Callable?", bond$callable))                    # Output: TRUE
# 
# # Updating attributes using setter methods
# bond$setCouponRate(0.06)
# bond$setIssueDate("2024-02-01")
# bond$setMaturityDate("2034-02-01")
# bond$setCountingConvention("Actual/Actual")
# 
# # Verifying the updated attributes
# print(paste("Updated Coupon Rate:", bond$getCouponRate()))          # Output: 0.06
# print(paste("Updated Issue Date:", bond$getIssueDate()))            # Output: "2024-02-01"
# print(paste("Updated Maturity Date:", bond$getMaturityDate()))      # Output: "2034-02-01"
# print(paste("Updated Counting Convention:", bond$getCountingConvention()))  # Output: "Actual/Actual"

