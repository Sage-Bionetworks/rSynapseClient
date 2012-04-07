## Get Objects from a Layer Entity
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

setGeneric(
  name = "getObject",
  def = function(entity, which){
    standardGeneric("getObject")
  }
)

setMethod(
  f = "getObject",
  signature = signature("LocationOwner", "character"),
  definition = function(entity, which){
    getObject(entity@location, which)
  }
)

setMethod(
  f = "getObject",
  signature = signature("Location", "character"),
  definition = function(entity, which){
    get(which, envir = entity@objects)
  }
)

