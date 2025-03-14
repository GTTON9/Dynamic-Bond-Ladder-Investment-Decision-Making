vRecur <- function(currDate,terminalDate,bpList,bondID){
  
  if((as.Date(terminalDate) - as.Date(currDate)) == 1){
    
    return( funcW(bpList[length(bpList)],currDate = currDate,terminalDate = terminalDate,bondID) )
    
  }else{
    
    currW <- funcW(bpList,currDate = currDate,terminalDate = terminalDate,bondID)
    currVal <- vRecur(as.character(as.Date(currDate) + 1),terminalDate,bpList,bondID)
    
    if(currW[[3]] > currVal[[3]]){
      
      return( currW )
      
    }else{
      
      return(currVal)
      
    }
  }
  
}


 
# # result <- vRecur(list(bp),currDate = bp$getCurrDate(),terminalDate = '2015-01-31',2)
# 
# start = Sys.time()
# result <- vRecur(list(bp),currDate = bp$getCurrDate(),terminalDate = '2022-01-31',2)
# end = Sys.time()
# 
# print(end - start)