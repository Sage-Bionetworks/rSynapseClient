# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "RObject",
  signature = "list",
  definition = function(entity, ...){
    Constructor("RObject", entity, ...)
  }
)

setMethod(
  f = "RObject",
  signature = "missing",
  definition = function(entity, ...){
    Constructor("RObject", ...)
  }
)

