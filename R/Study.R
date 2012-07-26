# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "Study",
  signature = "list",
  definition = function(entity, ...){
    Constructor("Study", entity, ...)
  }
)

setMethod(
  f = "Study",
  signature = "missing",
  definition = function(entity, ...){
    Constructor("Study", ...)
  }
)
