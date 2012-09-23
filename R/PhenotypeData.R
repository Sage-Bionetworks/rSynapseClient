# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "PhenotypeData",
  signature = "list",
  definition = function(entity, ...){
    Constructor("PhenotypeData", entity, ...)
  }
)

setMethod(
  f = "PhenotypeData",
  signature = "missing",
  definition = function(entity, ...){
    Constructor("PhenotypeData", ...)
  }
)

