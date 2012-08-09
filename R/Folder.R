# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "Folder",
  signature = "list",
  definition = function(entity){
    ee <- new("Folder")
    ee@properties <- entity
    ee@properties$entityType <- getSynapseTypeFromClass(as.character(class(ee)))
    ee
  }
)

setMethod(
  f = "Folder",
  signature = "missing",
  definition = function(){
    Folder(list())
  }
)

