# Define the bondType class
bondType <- setRefClass("bondType",
  fields = list(
    couponRate = "numeric",
    issueDate = "character",        # Assuming date stored as a string
    maturityDate = "character",      # Can convert to Date if needed
    period = "numeric",
    countingConvention = "character",
    callable = "logical"
  ),
  
  methods = list(
    initialize = function(couponRate, issueDate, maturityDate, period, countingConvention, callable = FALSE) {
      couponRate <<- couponRate
      issueDate <<- issueDate
      maturityDate <<- maturityDate
      period <<- period
      countingConvention <<- countingConvention
      callable <<- callable
    },
    
    getCouponRate = function() {
      return(couponRate)
    },
    
    setCouponRate = function(newRate) {
      couponRate <<- newRate
    },
    
    getIssueDate = function() {
      return(issueDate)
    },
    
    setIssueDate = function(newDate) {
      issueDate <<- newDate
    },
    
    getMaturityDate = function() {
      return(maturityDate)
    },
    
    setMaturityDate = function(newDate) {
      maturityDate <<- newDate
    },
    
    getCountingConvention = function() {
      return(countingConvention)
    },
    
    setCountingConvention = function(newConvention) {
      countingConvention <<- newConvention
    }
  )
)

# Define the bondType class (same as above)

# Create a new bondType object
bond <- bondType(couponRate = 0.05, 
                     issueDate = "2023-01-01", 
                     maturityDate = "2033-01-01", 
                     period = 6, 
                     countingConvention = "30/360", 
                     callable = TRUE)

# Accessing attributes using getter methods
print(paste("Coupon Rate:", bond$getCouponRate()))            # Output: 0.05
print(paste("Issue Date:", bond$getIssueDate()))              # Output: "2023-01-01"
print(paste("Maturity Date:", bond$getMaturityDate()))        # Output: "2033-01-01"
print(paste("Counting Convention:", bond$getCountingConvention()))  # Output: "30/360"
print(paste("Is Callable?", bond$callable))                    # Output: TRUE

# Updating attributes using setter methods
bond$setCouponRate(0.06)
bond$setIssueDate("2024-02-01")
bond$setMaturityDate("2034-02-01")
bond$setCountingConvention("Actual/Actual")

# Verifying the updated attributes
print(paste("Updated Coupon Rate:", bond$getCouponRate()))          # Output: 0.06
print(paste("Updated Issue Date:", bond$getIssueDate()))            # Output: "2024-02-01"
print(paste("Updated Maturity Date:", bond$getMaturityDate()))      # Output: "2034-02-01"
print(paste("Updated Counting Convention:", bond$getCountingConvention()))  # Output: "Actual/Actual"

