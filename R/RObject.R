# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "RObject",
  signature = "list",
  definition = function(entity){
    ee <- new("Data")
    ee@properties <- entity
    ee@properties$entityType <- getSynapseTypeFromClass(as.character(class(ee)))
    ee
  }
)

setMethod(
  f = "RObject",
  signature = "missing",
  definition = function(){
    Data(list())
  }
)

