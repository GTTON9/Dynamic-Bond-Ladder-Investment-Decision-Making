timeToMaturity <- function(trading_day, maturity_day, convention) {
  if (!inherits(trading_day, "Date") || !inherits(maturity_day, "Date")) {
    stop("Both trading_day and maturity_day must be Date objects.")
  }
  
  convention <- match.arg(convention, c("30/360", "actual/360", "actual/365", "actual/actual"))
  
  if (convention == "30/360") {
    adjust_date <- function(date) {
      lt <- as.POSIXlt(date)
      d <- lt$mday
      m <- lt$mon + 1
      y <- lt$year + 1900
      
      # Adjust day for 31st and end of February
      if (d == 31) d <- 30
      if (m == 2) {
        next_day <- as.POSIXlt(date + 1)
        if (next_day$mon + 1 != 2) d <- 30
      }
      list(day = d, month = m, year = y)
    }
    
    start_adj <- adjust_date(trading_day)
    end_adj <- adjust_date(maturity_day)
    
    days_count <- (end_adj$year - start_adj$year) * 360 +
      (end_adj$month - start_adj$month) * 30 +
      (end_adj$day - start_adj$day)
    
    # Convert days to years using 360-day year
    years_count <- days_count / 360
    return(abs(years_count))
  } else {
    # For all actual conventions, calculate actual days
    days_count <- abs(as.numeric(difftime(maturity_day, trading_day, units = "days")))
    
    if (convention == "actual/360") {
      years_count <- days_count / 360
    } else if (convention == "actual/365") {
      years_count <- days_count / 365
    } else if (convention == "actual/actual") {
      # Rigorous calculation for actual/actual
      years_count <- 0
      start_year <- as.POSIXlt(trading_day)$year + 1900
      end_year <- as.POSIXlt(maturity_day)$year + 1900
      
      for (year in start_year:end_year) {
        # Define start and end of the year
        year_start <- as.Date(paste0(year, "-01-01"))
        year_end <- as.Date(paste0(year, "-12-31"))
        
        # Determine the overlap between the period and the year
        period_start <- max(trading_day, year_start)
        period_end <- min(maturity_day, year_end)
        
        if (period_start > period_end) next  # No overlap
        
        # Calculate days in the year
        days_in_year <- ifelse(
          (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0),
          366, 365
        )
        
        # Add the fraction of the year
        years_count <- years_count + as.numeric(period_end - period_start) / days_in_year
      }
    }
    
    return(abs(years_count))
  }
}
sapply(X = as.Date(c('2023-01-01', '2024-01-01')), FUN = timeToMaturity, maturity_day = as.Date('2033-01-01'), convention = '30/360')

trading_day <- as.Date("2020-10-01")
maturity_day <- as.Date("2025-10-01")
convention <- "actual/actual"
time_to_maturity_value <- timeToMaturity(trading_day, maturity_day, convention)
print(paste("Time to maturity (in years):", time_to_maturity_value))

add_months <- function(date, h) {
  # Extract year, month, and day from the input date
  y <- as.numeric(format(date, "%Y"))
  m <- as.numeric(format(date, "%m"))
  d <- as.numeric(format(date, "%d"))
  # Compute new month and year
  new_m <- m + h
  new_y <- y + (new_m - 1) %/% 12
  new_m <- ((new_m - 1) %% 12) + 1
  
  # Get the first day of the new month
  first_of_month <- as.Date(sprintf("%04d-%02d-01", new_y, new_m))
  # Find the last day of the new month:
  # We get the first day of the following month, subtract one day.
  last_day <- as.numeric(format(seq(first_of_month, by = "month", length.out = 2)[2] - 1, "%d"))
  
  # Adjust the day if needed
  if (d > last_day) {
    d <- last_day
  }
  
  # Construct and return the new date
  as.Date(sprintf("%04d-%02d-%02d", new_y, new_m, d))
}


