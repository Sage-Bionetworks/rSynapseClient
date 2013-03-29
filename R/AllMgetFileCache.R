setMethod(
  f = "getFileCache",
  signature = signature("character", "character", "missing"),
  definition = function(archivePath, method){
    factory <- new("FileCacheFactory")
    getFileCache(archivePath, method, factory)
  }
)

setMethod(
  f = "getFileCache",
  signature = signature("character", "missing", "missing"),
  definition = function(archivePath){
    factory <- new("FileCacheFactory")
    getFileCache(archivePath, "load", factory=factory)
  }
)

setMethod(
  f = "getFileCache",
  signature = signature("missing", "missing", "missing"),
  definition = function(){
    factory <- new("FileCacheFactory")
    getFileCache(factory=factory)
  }
)

