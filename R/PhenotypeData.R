# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "PhenotypeData",
  signature = "list",
  definition = function(entity){
    ee <- new("PhenotypeData")
    ee@properties <- entity
    ee@properties$entityType <- getSynapseTypeFromClass(as.character(class(ee)))
    ee
  }
)

setMethod(
  f = "PhenotypeData",
  signature = "missing",
  definition = function(){
    Data(list())
  }
)

