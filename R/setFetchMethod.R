###
## used to change the the behavior of store entity for the entity associated with
## the fileCache object
###
setMethod(
  f = "setFetchMethod",
  signature = signature("character", "character", "missing"),
  definition = function(object, method){
    factory <- new("FileCacheFactory")
    setFetchMethod(object, method, factory)
  }
)
