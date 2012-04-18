# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
  f = "getEntityInstance",
  signature = signature("list"),
  definition = function(entity)
  {
    class <- getClassFromSynapseEntityType(entity$entityType)
    
    ## synapseEntity is the default
    if(is.null(class))
      class <- "SynapseEntity"
    
    if(class == "SynapseEntity"){
      if(!is.null(entity$locations) && length(entity$locations) > 0)
        class <- "SynapseLocationOwnerWithObjects"
    }
    
    ## call the appropriate constructor and pass the list
    ## representation of the entity
    ee <- do.call(class, list(entity = entity))
    ee@synapseWebUrl <- .buildSynapseUrl(propertyValue(ee, "id"))
    ee
  }
)
