# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "GenotypeData",
  signature = "list",
  definition = function(entity){
    ee <- new("GenotypeData")
    ee@properties <- entity
    ee@properties$entityType <- getSynapseTypeFromClass(as.character(class(ee)))
    ee
  }
)

setMethod(
  f = "GenotypeData",
  signature = "missing",
  definition = function(){
    Data(list())
  }
)

