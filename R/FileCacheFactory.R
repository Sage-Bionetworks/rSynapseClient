# TODO: Add comment
#
# Author: furia
###############################################################################


setMethod(
  f = "getFileCache",
  signature = signature("character"),
  definition = function(archivePath){
    if(length(archivePath) == 0L)
      stop("archivePath was null")

    factory <- new("FileCacheFactory")
    if(!file.exists(archivePath)){
      if(
        grepl(".zip$", tolower(archivePath)) ||
        grepl("tar.gz$", tolower(archivePath)) ||
        grepl("tar$", tolower(archivePath)) ||
        grepl("bz$", tolower(archivePath))
      ){
        fileCache <- FileCache(archiveFile=archivePath)
        ## should never reach here
        stop("archive file does not exist")
      }
      fileCache <- FileCache(cacheRoot=archivePath)
      assign(fileCache$cacheRoot, fileCache, envir=factory@env)
      return(fileCache)
    }
    archivePath <- normalizePath(archivePath, mustWork=TRUE)
    archivePath <- gsub("/+", "/", archivePath)
    archivePath <- gsub("/+$", "", archivePath)

    if(file.info(archivePath)$isdir){
      fileCache <- FileCache(cacheRoot=archivePath)
    }else{
      fileCache <- FileCache(archiveFile = archivePath)
    }

    ## check to see if the FileCache is already in the factory
    if(fileCache$cacheRoot %in% objects(factory@env))
      return(get(fileCache$cacheRoot, envir = factory@env))

    assign(fileCache$cacheRoot, fileCache, envir=factory@env)
    fileCache
  }
)

setMethod(
  f = "getFileCache",
  signature = signature("missing"),
  definition = function(){
    getFileCache(tempfile())
  }
)

setMethod(
  f = "moveFileCache",
  signature = signature("character", "character"),
  definition = function(from, to){
    factory <- new("FileCacheFactory")
    from <- normalizePath(from, mustWork=FALSE)
    to <- normalizePath(to, mustWork=FALSE)
    assign(to, get(from, envir=factory@env) ,envir=factory@env)
    rm(list=from, envir=factory@env)
  }
)


setMethod(
  f = "availFileCaches",
  signature("FileCacheFactory"),
  definition = function(factory){
    objects(factory@env)
  }
)

setMethod(
  f = "availFileCaches",
  signature = "missing",
  definition = function(){
    factory <- new("FileCacheFactory")
    objects(factory@env)
  }
)

setMethod(
  f = "resetFactory",
  signature = "FileCacheFactory",
  definition = function(factory){
    rm(list=availFileCaches(factory), envir=factory@env)
  }
)

setMethod(
  f = "removeFileCache",
  signature = "character",
  definition = function(path){
    factory <- new("FileCacheFactory")
    rm(list=normalizePath(path), envir=factory@env)
  }
)
