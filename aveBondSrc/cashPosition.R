cashPosition <- setRefClass("cashPosition",
  fields = list(
    surplus = 'numeric',
    deficit = 'numeric',
    fixedRate = 'numeric',
    currDate = 'character'
  ),
  
  methods = list(
    initialize = function(surplus = NA_real_,deficit = NA_real_,currDate = NA_character_
                          ,fixedRate = NA_real_) {
      .self$surplus <- surplus
      .self$deficit <- deficit
      .self$currDate <- currDate
      .self$fixedRate <- fixedRate
    },
    
    
    getSurplus = function(){
      return(.self$surplus)
    },
    
    setSurplus = function(newSurplus){
      .self$surplus <- newSurplus
    },
    
    getDeficit = function(){
      return(.self$deficit)
    },
    
    setDeficit = function(newDeficit){
      .self$deficit <- newDeficit
    },
    
    getCurrDate = function(){
      return(.self$currDate)
    },
    
    setCurrDate = function(newDate){
      .self$currDate <- newDate
    },
    
    getFixedRate = function(){
      return(.self$fixedRate)
    },
    
    setFixedRate = function(newRate){
      .self$fixedRate <- newRate
    },
    
    getPos = function(){
      return(.self$surplus + .self$deficit)
    },
    
    
    h1 = function(action,amt,moveForward = T){
      if(action == 'cashDown'){
        return(-amt)
      }
      
      else if(action == 'cashUp'){
        
        if(moveForward){
          newS <- .self$getSurplus() * exp(.self$getFixedRate())
        }
        else{
          newS <- .self$getSurplus()
        }
        
        return(-min(0,newS + h2(action,amt,moveForward = moveForward)))
      }
      
      else if(action == 'none'){
        return(0)
      }
      
      else {
        stop("Invalid action specified.")
      }
    },
    
    h2 = function(action,amt,moveForward = T){
      if(action == 'CashUp'){
        return(-amt)
      }
      
      else if(action == 'cashDown'){
        
        if(moveForward){
          newS <- .self$getDeficit() * exp(.self$getFixedRate())
        }
        else{
          newS <- .self$getDeficit()
        }
        
        return(-min(0,newS + h2(action,amt,moveForward = moveForward)))
      }
      
      else if(action == 'none'){
        return(0)
      }
      
      else {
        stop("Invalid action specified.")
      }
    },
    
    
    updateCashPos(action,amt,moveForward = T){
      
      currDeficit <- .self$getDeficit()
      currSurplus <- .self$getSurplus()
      
      if(moveForward){
        
        currFixedRate <- exp(.self$getFixedRate())
        newSurplus <- currSurplus * currFixedRate
        newDeficit <- currDeficit * currFixedRate

      }
      else{
        newSurplus <- currSurplus
        newDeficit <- currDeficit
      }
      
      .self$setDeficit(max(newDeficit + h1(action,amt,moveForward),0))
      .self$setSurplus(max(newSurplus + h2(action,amt,moveForward),0))
    }
    
  )
)
