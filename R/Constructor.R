## GENERIC CONSTRUCTOR WHICH IS CALLED BY ENTITY-SPECIFIC CONSTRUCTORS
#####

setMethod(
  f = "Constructor",
  signature = c("character", "list"),
  definition = function(classType, entity, ...){
    
    ## GRAB NAMED ARGUMENTS AND ADD TO ENTITY LIST
    argList <- list(...)
    entity <- c(entity, argList)
    
    ee <- new(classType)
    ee@properties <- entity
    ee@properties$entityType <- getSynapseTypeFromClass(classType)
    ee
  }
)

setMethod(
  f = "Constructor",
  signature = c("character", "missing"),
  definition = function(classType, entity, ...){
    
    ## GRAB NAMED ARGUMENTS IF NOT ENTITY LIST PASSED
    argList <- list(...)
    
    if(length(argList) > 0){
      if(any(names(argList) == ""))
        stop(sprintf("Arguments passed to %s must be named", classType))
    }
    Constructor(classType, argList)
    
  }
)
