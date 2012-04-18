# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
  f = "Data",
  signature = "list",
  definition = function(entity){
    ee <- new("Data")
    ee@properties <- entity
    ee@properties$entityType <- getSynapseTypeFromClass(as.character(class(ee)))
    ee
  }
)

