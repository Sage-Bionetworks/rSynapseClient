# TODO: Add comment
#
# Author: furia
###############################################################################

setMethod(
  f = "availFileCaches",
  signature("character", "FileCacheFactory"),
  definition = function(method, factory){
    objects(slot(factory, method))
  }
)

setMethod(
  f = "resetFactory",
  signature = "FileCacheFactory",
  definition = function(factory){
    for(sn in slotNames(factory)){
      ss = slot(factory, sn)
      if("environment" %in% class(ss))
        rm(list=availFileCaches(sn, factory), envir=ss)
    }
  }
)

setMethod(
  f = "availFileCaches",
  signature("missing", "FileCacheFactory"),
  definition = function(factory){
    avail <- NULL
    for(sn in slotNames(factory)){
      ss = slot(factory, sn)
      if("environment" %in% class(ss))
        avail <- c(avail, availFileCaches(sn, factory))
    }
    avail
  }
)

setMethod(
  f = "setFetchMethod",
  signature = signature("character", "character", "FileCacheFactory"),
  definition = function(object, method, factory){
    object <- fixFilePath(object)
    if(!(object %in% availFileCaches(factory=factory)))
      stop(sprintf("invalid archivePath: %s", object))
    if(method != getFetchMethod(object, factory)){
      oldMethod <- getFetchMethod(object, factory)
      assign(object, get(object, envir=slot(factory, oldMethod)), envir=slot(factory, method))
      rm(list=object, envir=slot(factory, oldMethod))
    }
  }
)

setMethod(
  f = "removeFileCache",
  signature = signature("character", "missing", "FileCacheFactory"),
  definition = function(object, factory){
    removeFileCache(object, getFetchMethod(object, factory), factory)
  }
)

setMethod(
  f = "removeFileCache",
  signature = signature("character", "character", "FileCacheFactory"),
  definition = function(object, method, factory){
    object <- fixFilePath(object)
    ## double check fetch method
    if(method != getFetchMethod(object, factory))
      stop("wrong fetch method indicated")
    rm(list=object, envir=slot(factory, method))
  }
)

setMethod(
  f = "getFetchMethod",
  signature = signature("character", "FileCacheFactory"),
  definition = function(object, factory){
    object <- fixFilePath(object)
    method <- NULL
    for(sn in slotNames(factory)){
      ss <- slot(factory, sn)
      if("environment" %in% class(ss) && object %in% objects(ss))
        method <- c(method, sn)
    }
    if(length(method) > 1L)
      stop("entity had more multiple fetch methods. Illegal state")
    method
  }
)

setMethod(
  f = "getFileCache",
  signature = signature("missing", "missing", "FileCacheFactory"),
  definition = function(factory){
    getFileCache(tempfile(), "load", factory)
  }
)

setMethod(
  f = "getFileCache",
  signature = signature("character", "missing", "FileCacheFactory"),
  definition = function(archivePath, factory){
    getFileCache(archivePath, "load", factory)
  }
)

setMethod(
  f = "getFileCache",
  signature = signature("character", "character", "FileCacheFactory"),
  definition = function(archivePath, method, factory){
    if(length(archivePath) == 0L)
      stop("archivePath was null")
    if(length(archivePath) > 1L)
      stop("must provide a single file path")

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
      if(getFileCacheName(fileCache) %in% setdiff(availFileCaches(factory=factory), availFileCaches(method, factory)))
        stop(sprintf("could not add fileCache to factory with this method: %s", method))
      assign(getFileCacheName(fileCache), fileCache, envir=slot(factory, method))
      return(fileCache)
    }
    archivePath <- fixFilePath(archivePath, mustWork=TRUE)

    if(file.info(archivePath)$isdir){
      fileCache <- FileCache(cacheRoot=archivePath)
    }else{
      fileCache <- FileCache(archiveFile = archivePath)
    }

    ## check to see if the FileCache is already in the factory
    if(getFileCacheName(fileCache) %in% availFileCaches(factory=factory)){
      ## determine if the fetch method is being changed
      if(getFetchMethod(fileCache, factory) != method)
        setFetchMethod(fileCache, method, factory)

      return(get(getFileCacheName(fileCache), envir = slot(factory, method)))
    }

    if(getFileCacheName(fileCache) %in% setdiff(availFileCaches(factory=factory), availFileCaches(method, factory)))
      stop(sprintf("could not add fileCache to factory with this method: %s", method))
    assign(getFileCacheName(fileCache), fileCache, envir=slot(factory, method))
    fileCache
  }
)