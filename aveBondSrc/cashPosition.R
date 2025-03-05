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
    }
  )
)
