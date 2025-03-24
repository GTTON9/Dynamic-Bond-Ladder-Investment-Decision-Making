library(lubridate)

monthsBtw <- function(sDate,eDate){
  
  if(!is.Date(sDate)){
    start = as.Date(sDate)
  }else{
    start = sDate
  }
  
  if(!is.Date(eDate)){
    end = as.Date(eDate)
  }else{
    end = eDate
  }
  
  end = as.Date(eDate)
  
  forced_start <- as.Date(paste0(format(start, "%Y-%m"), "-01"))
  forced_end <- as.Date(paste0(format(end, "%Y-%m"), "-01")) %m+% months(2)
  
  
  seq_dates <- seq.Date(forced_start, forced_end, by = "month")
  return(format(seq_dates, "%Y-%m-%d"))
  
}
