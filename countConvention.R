time_to_maturity <- function(day_count_convention, trading_date, maturity_date) {
  # Convert inputs to Date objects
  trading_date <- as.Date(trading_date, format = "%Y-%m-%d")
  maturity_date <- as.Date(maturity_date, format = "%Y-%m-%d")
  
  # Calculate the difference in days
  days_diff <- as.numeric(difftime(maturity_date, trading_date, units = "days"))
  
  # Calculate time to maturity based on the day-count convention
  if (day_count_convention == "Actual/Actual") {
    # Actual/Actual: Use actual days and actual days in the year
    year_diff <- as.numeric(format(maturity_date, "%Y")) - as.numeric(format(trading_date, "%Y"))
    
    start_year <- as.numeric(format(trading_date, "%Y"))
    end_year <- as.numeric(format(maturity_date, "%Y"))
    
    # Calculate the actual number of days in the years
    days_in_start_year <- as.numeric(difftime(as.Date(paste0(start_year, "-12-31")), 
                                              as.Date(paste0(start_year, "-01-01")), units = "days")) + 1
    days_in_end_year <- as.numeric(difftime(as.Date(paste0(end_year, "-12-31")), 
                                            as.Date(paste0(end_year, "-01-01")), units = "days")) + 1
    
    # Calculate the fraction of the year
    if (year_diff == 0) {
      time_to_maturity <- days_diff / days_in_start_year
    } else {
      time_to_maturity <- year_diff + (days_diff / days_in_end_year)
    }
    
  } else if (day_count_convention == "30/360") {
    # 30/360: Assume 30 days in a month and 360 days in a year
    day_trading <- min(30, as.numeric(format(trading_date, "%d")))
    day_maturity <- min(30, as.numeric(format(maturity_date, "%d")))
    
    month_trading <- as.numeric(format(trading_date, "%m"))
    month_maturity <- as.numeric(format(maturity_date, "%m"))
    
    year_trading <- as.numeric(format(trading_date, "%Y"))
    year_maturity <- as.numeric(format(maturity_date, "%Y"))
    
    days_diff <- (year_maturity - year_trading) * 360 + 
      (month_maturity - month_trading) * 30 + 
      (day_maturity - day_trading)
    
    time_to_maturity <- days_diff / 360
    
  } else if (day_count_convention == "Actual/360") {
    # Actual/360: Use actual days but assume 360 days in a year
    time_to_maturity <- days_diff / 360
    
  } else if (day_count_convention == "Actual/365") {
    # Actual/365: Use actual days but assume 365 days in a year
    time_to_maturity <- days_diff / 365
    
  } else {
    stop("Invalid day-count convention. Choose from 'Actual/Actual', '30/360', 'Actual/360', or 'Actual/365'.")
  }
  
  return(time_to_maturity)
}

# Example usage:
# trading_date <- "2023-10-01"
# maturity_date <- "2025-10-01"
# day_count_convention <- "Actual/Actual"
# time_to_maturity_value <- time_to_maturity(day_count_convention, trading_date, maturity_date)
# print(paste("Time to maturity (in years):", time_to_maturity_value))
