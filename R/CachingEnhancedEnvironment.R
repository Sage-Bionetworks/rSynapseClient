## An EnhancedEnvironment that automatically manages a disk cache containing
## serialized versions of it's objects.
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

##
## initialize method for CachingEnhancedEnvironment class. By default, serialized
## objects are stored in a subdirectory of the File cache named ".R_OBJECTS".
##
setMethod(
  f = "initialize",
  signature = "CachingEnhancedEnvironment",
  definition = function(.Object){
    ## By default, store the files in subdirectory of the FileCache
    .Object@env = new.env(parent = emptyenv())
    .Object@cachePrefix <- ".R_OBJECTS/"
    .Object@fileCache <- FileCache()
    .Ojbect@cacheSuffix <- "rbin"
    .Object@cacheTmpSuffix <- "rbin.tmp"
    .Object 
  }
)

##
## Over-ride the addObject method inherited from EnhancedEnvironment. The method
## for CachingEnhancedEnvironment does the same thing as the one for EnhancedEnvironment
## but it also caches a binary to disk. Failure to cache will prevent the object from
## being added to the environment.
##
setMethod(
  f = "addObject",
  signature = signature("CachingEnhancedEnvironment", "ANY", "character", "missing"),
  definition = function(owner, object, name){
    oldClass <- class(owner)
    class(owner) <- "EnhancedEnvironment"
    owner <- addObject(owner, object, name)
    class(owner) <- oldClass
    owner <- tryCatch(
      .cacheObject(owner, name),
      error = function(e){
        deleteObject(owner, name)
        stop(e)
      }
    )
    invisible(owner)
  }
)

##
## Over-ride the deleteObject method inherited from EnhancedEnvironment. The method
## for CachingEnhancedEnvironment does the same thing as the one for EnhancedEnvironment
## but it also deletes the cached binary from disk. Failure to delete the cache file
## will print a warning but will not prevent the object from being delted from the
## environment.
##
setMethod(
  f = "deleteObject",
  signature = signature("CachingEnhancedEnvironment", "character"),
  definition = function(owner, which){
    tryCatch(
      .deleteCacheFile(owner, which),
      error = function(e){
        warning(sprintf("Unable to remove cached binary for '%s' object: %s"), which, e)
      }
    )
    oldClass <- class(owner)
    class(owner) <- "EnhancedEnvironment"
    owner <- deleteObject(owner, name)
    class(owner) <- oldClass
    invisible(owner)
  }
)

##
## List cache files for the class. Returns the paths relative to the cacheDirectory
## of File cache
##
setMethod(
  f = "files",
  signature = "CachingEnhancedEnvironment",
  definition = function(object){
    files <- owner@fileCache$files()
    indx <- grep(pattern, files, all.files = TRUE)
    if(length(indx == 0L))
      return(character())
    files <- files[indx]  
  }
)

##
## cache the object to disk. use the FileCache object to store metaData
## retarding the cached file
##
setMethod(
  f = ".cacheObject",
  signature = signature("CachingEnhancedEnvironment", "character"),
  definition = function(owner, objectName)
  {
    if(!(objectName %in% names(owner)))
      stop("Could not cache object to disk because it was not present in the environment.")
    destFile <- .generateCacheFileName(owner, objectName)
    
    ## add meta data about this object to the FileCache object
    ## this method should be improved once the addFile method of
    ## FileCache is improved to add directly from a connection
    ## for now, just add metadata to the FileCache and handle cache
    ## file creation then from this method
    tryCatch(
      save(list = objectName, envir=as.environment(owner), file = destFile),
      error = function(e){
        stop(e)
      }
    )
    addFileMetaData(object@fileCache, destFile, destFile)
    
    ## cache the metadata to disk
    object@fileCache$cacheFileMetaData()
    invisible(owner)
  }
)

##
## Delete the cache file for the given object name
##
setMethod(
  f = ".deleteCacheFile",
  signature = signature("CachingEnhancedEnvironment", "character"),
  definition = function(owner, objectName){
    owner@fileCache <- deleteFile(owner@fileCache, .generateCacheFileRelativePath(owner, objectName))
    invisible(owner)
  }
)

##
## generate the full file path the the cached file for the give object
##
setMethod(
  f = ".generateCacheFileName",
  signature = signature("CachingEnhancedEnvironment", "character", "ANY"),
  definition = function(owner, objectName, suffix){
    
    filePath <- file.path(owner@fileCache$getCacheDir(), .generateCacheFileRelativePath(owner, objectName, suffix))
    
    ## this is the cachedir where binary objects are cached on disk
    cacheDir <- file.path(owner@fileCache$getCacheDir(), owner@cachePrefix)
    
    ## if the prefix isn't a directory, set the cache dir to the root
    if(!grepl("/+$", owner@cachePrefix))
      cacheDir <- owner@fileCache$getCacheDir()
    
    attr(filePath, "cacheDir") <- cacheDir
    filePath
  }
)

##
## generate the relative path for an object
##
setMethod(
  f = ".generateCacheFileRelativePath",
  signature = singature("CachingEnhancedEnvironment", "character", "ANY"),
  definition = function(owner, objectName, suffix){
    if(missing(suffix))
      suffix <- owner@cacheSuffix
    if(!is.character(suffix))
      stop("suffix must be a character")
    
    sprintf("%s%s.%s", owner@cachePrefix, objectName, suffix)
  }
)

##
## generate the relative path for an object
##
setMethod(
  f = ".generateTmpCacheFileRelativePath",
  signature = singature("CachingEnhancedEnvironment", "character", "ANY"),
  definition = function(owner, objectName, suffix){
    if(missing(suffix))
      suffix <- owner@tmpCacheSuffix
    if(!is.character(suffix))
      stop("suffix must be a character")
    
    generateCacheFileName(object, destName, suffix)
  }
)

##
## generate the temporaty cache file name for an object
##
setMethod(
  f = ".generateTmpCacheFileName",
  signature = signature("CachingEnhancedEnvironment", "character"),
  definition = function(entity, objectName)
  {
    .generateCacheFileRelativePath(entity, objectName, owner@cacheTmpSuffix)
  }
)

##
## move the object's cache file to a temporary location
##
setMethod(
  f = ".tmpCacheObject",
  signature = signature("CachingEnhancedEnvironment", "character"),
  definition = 
    function(object, objectName)
  {
    owner@fileCache <- moveFile(object@fileCache, 
      .generateCacheFileRelativePath(object, objectName), 
      .generateTmpCacheFileRelativePath(object, objectName)
    )
    invisible(object)
  }
)

##
## move the object's temporary file to it's new destination
##
setMethod(
  f = ".renameCacheObjectFromTmp",
  signature = signature("CachingEnhancedEnvironment", "character", "character"),
  definition = function(object, srcName, destName){
    owner@fileCache <- moveFile(object@fileCache, 
      .generateTmpCacheFileRelativePath(object, srcName), 
      .generateCacheFileRelativePath(object, destName)
    )
    invisible(owner)
  }
)

##
## delete the object's temporary file
##
setMethod(
  f = ".deleteTmpCacheFile",
  signature = signature("CachingEnhancedEnvironment", "character"),
  definition = function(owner, objectName){
    owner@fileCache <- deleteFile(object@fileCache, .generateTmpCacheFileRelativePath(object, objectName))
    invisible(owner)
  }
)

##
## load cached objects from disk
##
setMethod(
  f = ".loadCachedObjects",
  signature = signature("CachingEnhancedEnvironment"),
  definition = function(owner){
    
    ## get the cached files
    files <- file.path(objects@fileCache$getCacheDir(), files(owner))
    if(length(indx == 0L))
      invisible(owner)
    
    ## load the files into the environment
    lapply(
      files, FUN = function(filepath){
        load(filepath, envir = as.environment(owner))
      }
    )
    invisible(owner)
  }
)
