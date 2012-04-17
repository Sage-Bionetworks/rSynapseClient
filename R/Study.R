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
    ee
  }
)

