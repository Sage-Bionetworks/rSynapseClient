# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
  f = "getChildEntities",
  signature = "Entity",
  definition = function(entity){
    qry <- "Select id, type from entity"
    stop("I'm broken")
  }
)
