# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
  f = "Data",
  signature = "list",
  definition = function(entity, ...){
    Constructor("Data", entity, ...)
  }
)

setMethod(
  f = "Data",
  signature = "missing",
  definition = function(entity, ...){
    Constructor("Data", ...)
  }
)
