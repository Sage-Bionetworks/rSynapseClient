# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "Study",
  signature = "list",
  definition = function(entity){
    ee <- new("Study")
    ee@properties <- entity
    ee@properties$entityType <- getSynapseTypeFromClass(as.character(class(ee)))
    ee
  }
)

setMethod(
    f = "Study",
    signature = "missing",
    definition = function(){
      Study(list())
    }
)
