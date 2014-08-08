# These are the methods for TypedList
# 
# Author: brucehoff
###############################################################################

setMethod(
  f = "$",
  signature = "TypedList",
  definition = function(x, name){
    x@content[[name]]
  }
)

validateTypedListElement<-function(typedList, value) {
  if (!is(value, typedList@type)) stop(sprintf("Expected %s but found %s.", typedList@type, class(value)))
}

setReplaceMethod("$",
  signature = "TypedList",
  definition = function(x, name, value) {
    validateTypedListElement(x, value)
    x@content[[name]]<-value
    x
  }
)

setMethod(
  f = "[[",
  signature = "TypedList",
  definition = function(x, i, j, ...){
    x@content[[i]]
  }
)

setReplaceMethod("[[", 
  signature = signature(
    x = "TypedList",
    i = "character"
  )
  ,
  function(x, i, value) {
    validateTypedListElement(x, value)
    x@content[[i]]<-value
    x
  }
)

setReplaceMethod("[[", 
  signature = signature(
    x = "TypedList",
    i = "integer"
  )
  ,
  function(x, i, value) {
    validateTypedListElement(x, value)
    x@content[[i]]<-value
    x
  }
)

setReplaceMethod("[[", 
  signature = signature(
    x = "TypedList",
    i = "numeric"
  )
  ,
  function(x, i, value) {
    validateTypedListElement(x, value)
    x@content[[i]]<-value
    x
  }
)

setMethod(
  f = "add",
  signature = signature("TypedList", "ANY"),
  definition = function(x, value) {
    validateTypedListElement(x, value)
    x@content[[1+length(x@content)]]<-value
    x
  }
)

setMethod(
  f = "set",
  signature = signature("TypedList", "list"),
  definition = function(x, values) {
    # make sure all values have the right type
    lapply(X=values, FUN=function(value){validateTypedListElement(x, value)})
    x@content<-values
    x
  }
)



setMethod(
  f = "length",
  signature = "TypedList",
  definition = function(x) {
    length(x@content)
  }
)

setMethod(
  f = "getList",
  signature = "TypedList",
  definition = function(x) {
    x@content
  }
)

