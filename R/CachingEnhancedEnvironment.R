# TODO: Add comment
# 
# Author: mfuria
###############################################################################

setMethod(
  f = "initialize",
  signature = "CachingEnhancedEnvionment",
  definition = function(.Object){
    ## By default, store the files in subdirectory of the FileCache
    .Object@env = new.env(parent = emptyenv())
    .Object@cachePrefix <- ".R_OBJECTS/"
    .Object@fileCache <- FileCache()
    .Object 
  }
)

## general purpose function for adding objects and caching to file
.doAddObjectWithCache <-
    function(owner, object, name)
{
  owner$objects[[name]] <- object
  tryCatch(
      .cacheObject(owner, name),
      error = function(e){
        deleteObject(owner, name)
        stop(e)
      }
  )
  owner
}

setMethod(
    f = "deleteObject",
    signature = signature("EnhancedEnvironment", "character"),
    definition = function(owner, which){
      rm(list=which, envir=as.envronment(owner))
    }
)

## generate the full file path the the cached file for the give object
setMethod(
    f = .generateCacheFileName,
    signature = signature("CachingEnhancedEnvironment", "character", "ANY"),
    definition = function(owner, objectName, suffix){
      if(missing(suffix))
        suffix <- "rbin"

      if(!is.character(suffix))
        stop("suffix must be a character")
      
      filePath <- file.path(owner@fileCache$getCacheDir(), sprintf("%s%s.%s", owner@cachePrefix, objectName, kSuffix))
      
      ## this is the cachedir where binary objects are cached on disk
      cacheDir <- file.path(owner@fileCache$getCacheDir(), owner@cachePrefix)
      
      ## if the prefix isn't a directory, set the cache dir to the root
      if(!grepl("/+$", owner@cachePrefix))
        cacheDir <- owner@fileCache$getCacheDir()
      
      attr(filePath, "cacheDir") <- cacheDir
      filePath
    }
)

## generate the temporaty cache file name
setMethod(
    f = ".generateTmpCacheFileName",
    signature = signature("CachingEnhancedEnvironment", "character"),
    definition = function(entity, objectName)
    {
      kSuffix <- "rbin.tmp"
      .generateCacheFileName(entity, objectName, kSuffix)
    }
)


## cache the object to disk
setMethod(
    f = ".cacheObject",
    signature = signature("CachingEnhancedEnvironment", "character"),
    definition = function(owner, objectName)
    {
      destFile <- .generateCacheFileName(owner, objectName)
      save(list = objectName, envir=as.environment(owner), file = destFile)
    }
)

## move the object's cache file to a temporary location
setMethod(
    f = ".tmpCacheObject",
    signature = signature("CachingEnhancedEnvironment", "character"),
    definition = 
        function(object, objectName)
    {
      if(!file.exists(.generateCacheFileName(object, objectName)))
        stop("source file does not exist")
      file.rename(.generateCacheFileName(object, objectName), .generateTmpCacheFileName(object, objectName))
    }
)

## move the object's temporary file to it's new destination
setMethod(
    f = ".renameCacheObjectFromTmp",
    signature = signature("CachingEnhancedEnvironment", "character", "character"),
    definition = function(object, srcName, destName){
      file.rename(.generateTmpCacheFileName(object, srcName), .generateCacheFileName(object, destName))
    }
)

setMethod(
    f = ".deleteTmpCacheFile",
    signature = signature("CachingEnhancedEnvironment", "character"),
    definition = function(owner, objectName){
      unlink(.generateTmpCacheFileName(owner, objectName))
    }
)

setMethod(
    f = ".deleteCacheFile",
    signature = signature("CachingEnhancedEnvironment", "character"),
    definition = function(owner, objectName){
      unlink(.generateCacheFileName(owner, objectName))
    }
)

setMethod(
    f = ".loadCachedObjects",
    signature = signature("CachingEnhancedEnvironment"),
    definition = function(owner){
      if(grepl("/+$", owner@cachePrefix)){
        files <- list.files(file.path(owner@fileCache$getCacheDir(), owner@cachePrefix), full.names=TRUE, pattern = "rbin$")
      }else{
        pattern <- sprintf("%s.+rbin$", owner@cachePrefix)
        files <- list.files(owner@fileCache$getCacheDir(), full.names=TRUE, pattern = pattern)
      }

      lapply(
          files, FUN = function(filepath){
            load(filepath, envir = as.environment(owner))
          }
      )
      invisible(owner)
    }
)
