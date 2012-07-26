# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "Folder",
  signature = "list",
  definition = function(entity, ...){
    Constructor("Folder", entity, ...)
  }
)

setMethod(
  f = "Folder",
  signature = "missing",
  definition = function(entity, ...){
    Constructor("Folder", ...)
  }
)

