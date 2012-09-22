setMethod(
  f = "removeFileCache",
  signature = signature("character", "missing", "missing"),
  definition = function(object){
    method <- getFetchMethod(object)
    removeFileCache(object, method)
  }
)

setMethod(
  f = "removeFileCache",
  signature = signature("character", "character", "missing"),
  definition = function(object, method){
    factory <- new("FileCacheFactory")
    removeFileCache(object, method, factory)
  }
)



