# TODO: Add comment
# 
# Author: furia
###############################################################################


setMethod(
  f = "getFileCache",
  signature = signature("character"),
  definition = function(archivePath){
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
    if(file.path(fileCache$cacheRoot, fileCache$archiveFile) %in% objects(factory@env))
      return(get(file.path(fileCache$cacheRoot, fileCache$archiveFile), envir = factory@env))
    
    assign(file.path(fileCache$cacheRoot, fileCache$archiveFile), fileCache, envir=factory@env)
    fileCache
  }
)

setMethod(
  f = "getFileCache",
  signature = signature("missing"),
  definition = function(){
    FileCache()
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
