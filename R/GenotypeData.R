# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "GenotypeData",
  signature = "list",
  definition = function(entity, ...){
    Constructor("GenotypeData", entity, ...)
  }
)

setMethod(
  f = "GenotypeData",
  signature = "missing",
  definition = function(entity, ...){
    Constructor("GenotypeData", ...)
  }
)

