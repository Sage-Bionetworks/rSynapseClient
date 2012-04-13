# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
  f = "propertyNames",
  signature = "SimplePropertyOwner",
  definition = function(object){
    propertyNames(object@properties)
  }
)

setMethod(
  f = "propertyValues",
  signature = "SimplePropertyOwner",
  definition = function(object){
    values <- lapply(propertyNames(object@properties), function(n){
        getProperty(object,n)
      }
    )
    unlist(values)
  }
)
