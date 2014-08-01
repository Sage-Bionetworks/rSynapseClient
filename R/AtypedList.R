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

setReplaceMethod("$",
  signature = "TypedList",
  definition = function(x, name, value) {
    if (!is(value, x@type)) stop(sprintf("Expected %s but found %s.", x@type, class(value)))
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
    if (!is(value, x@type)) stop(sprintf("Expected %s but found %s.", x@type, class(value)))
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
    if (!is(value, x@type)) stop(sprintf("Expected %s but found %s.", x@type, class(value)))
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
    if (!is(value, x@type)) stop(sprintf("Expected %s but found %s.", x@type, class(value)))
    x@content[[i]]<-value
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

