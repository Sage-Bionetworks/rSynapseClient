###
## return a list of the fileCache objects that have been instantiated
## during this session
###
setMethod(
  f = "availFileCaches",
  signature = signature("missing", "missing"),
  definition = function(){
    factory <- new("FileCacheFactory")
    availFileCaches(factory=factory)
  }
)

setMethod(
  f = "availFileCaches",
  signature = signature("character", "missing"),
  definition = function(method){
    factory <- new("FileCacheFactory")
    availFileCaches(method, factory)
  }
)

