## Method for getting parent entity
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setMethod(
  f = "getParentEntity",
  signature = "numeric",
  definition = function(entity){
    getParentEntity(as.character(entity))
  }
)

setMethod(
  f = "getParentEntity",
  signature = "character",
  definition = function(entity){
    entity <- getEntity(entity)
    getEntity(propertyValue(entity, "parentId"))
  }
)
