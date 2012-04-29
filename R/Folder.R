# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "Folder",
  signature = "list",
  definition = function(entity){
    ee <- new("Data")
    ee@properties <- entity
    ee@properties$entityType <- getSynapseTypeFromClass(as.character(class(ee)))
    ee
  }
)

setMethod(
  f = "Folder",
  signature = "missing",
  definition = function(){
    Data(list())
  }
)

