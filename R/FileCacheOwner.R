setMethod(
  f = "initialize",
  signature = "FileCacheOwner",
  definition = function(.Object){
    .Object@fileCache <- new("FileCache")
    .Object
  }
)

setMethod(
  f = "setFileCache",
  signature = signature("FileCacheOwner", "FileCache"),
  definition = function(owner, fileCache){
    owner@fileCache <- fileCache
    owner
  }
)

setMethod(
  f = "setCacheRoot",
  signature = signature("FileCacheOwner", "character", "logical", "missing"),
  definition = function(object, path, clean){
    setCacheRoot(object@fileCache, path, clean)
    invisible(object)
  }
)

setMethod(
  f = "addFile",
  signature = signature("FileCacheOwner", "character", "missing"),
  definition = function(entity,file, path){
    entity@fileCache <- addFile(entity@fileCache, file)
    invisible(entity)
  }
)

setMethod(
    f = "addFile",
    signature = signature("FileCacheOwner", "character", "character"),
    definition = function(entity,file, path){
      entity@fileCache <- addFile(entity@fileCache, file, path)
      invisible(entity)
    }
)

setMethod(
    f = "deleteFile",
    signature = signature("FileCacheOwner", "character"),
    definition = function(entity,file){
      entity@fileCache <- deleteFile(entity@fileCache, file)
      invisible(entity)
    }
)

setMethod(
    f = "moveFile",
    signature = signature("FileCacheOwner", "character", "character"),
    definition = function(entity,src, dest){
      entity@fileCache <- moveFile(entity@fileCache, src, dest)
      invisible(entity)
    }
)

setMethod(
  f = "cacheDir",
  signature = "FileCacheOwner",
  definition = function(object){
    object@fileCache$getCacheDir()
  }
)

setMethod(
  f = "files",
  signature = "FileCacheOwner",
  definition = function(object){
    object@fileCache$files()
  }
)

setMethod(
  f = "setPackageName",
  signature = signature(env = "FileCacheOwner"),
  definition = function(pkg, env)
  {
    if(missing(pkg))
      pkg <- basename(tempfile(pattern=as.character(class(env))))
    setPackageName(pkg = pkg, env = env@objects)
  }
)

setMethod(
  f = "setFetchMethod",
  signature = signature("FileCacheOwner", "character"),
  definition = function(object, method){
    setFetchMethod(object@fileCache, method)
  }
)

setMethod(
  f = "getFetchMethod",
  signature = signature("FileCacheOwner"),
  definition = function(object){
    getFetchMethod(object@fileCache)
  }
)
