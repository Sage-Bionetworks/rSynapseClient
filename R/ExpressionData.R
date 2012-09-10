# TODO: Add comment
# 
# Author: mfuria
###############################################################################


setGeneric(
  name = "ExpressionData",
  def = function(entity, ...){
    standardGeneric("ExpressionData")
  }
)

setMethod(
  f = "ExpressionData",
  signature = "missing",
  definition = function(entity, ...){
    Constructor("ExpressionData", ...)
  }
)

setMethod(
  f = "ExpressionData",
  signature = "list",
  definition = function(entity, ...){
    Constructor("ExpressionData", entity, ...)
  }
)
