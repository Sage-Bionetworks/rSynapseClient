# TODO: Add comment
# 
# Author: mfuria
###############################################################################


setGeneric(
  name = "ExpressionData",
  def = function(entity){
    standardGeneric("ExpressionData")
  }
)

setMethod(
    f = "ExpressionData",
    signature = "missing",
  definition = function(entity){
    ExpressionData(list())
  }
)

setMethod(
  f = "ExpressionData",
  signature = "list",
  definition = function(entity){
    ee <- new("ExpressionData")
    ee@properties <- entity
    ee@properties$entityType <- getSynapseTypeFromClass(as.character(class(ee)))
    ee
  }
)
