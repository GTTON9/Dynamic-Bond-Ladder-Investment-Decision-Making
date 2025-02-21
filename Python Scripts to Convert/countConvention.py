from datetime import datetime

def time_to_maturity(day_count_convention, trading_date, maturity_date):
    # Convert inputs to datetime objects
    trading_date = datetime.strptime(trading_date, "%Y-%m-%d")
    maturity_date = datetime.strptime(maturity_date, "%Y-%m-%d")
    
    # Calculate the difference in days
    days_diff = (maturity_date - trading_date).days
    
    # Calculate time to maturity based on the day-count convention
    if day_count_convention == "Actual/Actual":
        # Actual/Actual: Use actual days and actual days in the year
        year_diff = maturity_date.year - trading_date.year
        start_year = trading_date.year
        end_year = maturity_date.year
        
        # Calculate the actual number of days in the years
        days_in_start_year = (datetime(start_year, 12, 31) - datetime(start_year, 1, 1)).days + 1
        days_in_end_year = (datetime(end_year, 12, 31) - datetime(end_year, 1, 1)).days + 1
        
        # Calculate the fraction of the year
        if year_diff == 0:
            time_to_maturity = days_diff / days_in_start_year
        else:
            time_to_maturity = year_diff + (days_diff / days_in_end_year)
    
    elif day_count_convention == "30/360":
        # 30/360: Assume 30 days in a month and 360 days in a year
        day_trading = min(30, trading_date.day)
        day_maturity = min(30, maturity_date.day)
        
        month_trading = trading_date.month
        month_maturity = maturity_date.month
        
        year_trading = trading_date.year
        year_maturity = maturity_date.year
        
        days_diff = (year_maturity - year_trading) * 360 + \
                    (month_maturity - month_trading) * 30 + \
                    (day_maturity - day_trading)
        
        time_to_maturity = days_diff / 360
    
    elif day_count_convention == "Actual/360":
        # Actual/360: Use actual days but assume 360 days in a year
        time_to_maturity = days_diff / 360
    
    elif day_count_convention == "Actual/365":
        # Actual/365: Use actual days but assume 365 days in a year
        time_to_maturity = days_diff / 365
    
    else:
        raise ValueError("Invalid day-count convention. Choose from 'Actual/Actual', '30/360', 'Actual/360', or 'Actual/365'.")
    
    return time_to_maturity

# Example usage
# trading_date = "2023-10-01"
# maturity_date = "2025-10-01"
# day_count_convention = "Actual/Actual"

# time_to_maturity_value = time_to_maturity(day_count_convention, trading_date, maturity_date)
# print(f"Time to maturity (in years): {time_to_maturity_value}")