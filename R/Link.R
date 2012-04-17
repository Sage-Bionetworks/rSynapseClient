# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "Link",
  signature = "list",
  definition = function(entity){
    ee <- new("Link")
    ee@properties <- entity
    ee
  }
)
