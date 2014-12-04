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
    .Object@env = new.env()
    .Object@cachePrefix <- synapseObjectCache()
    .Object@fileCache <- FileCache()
    .Object@cacheSuffix <- "rbin"
    .Object@cacheTmpSuffix <- "rbin.tmp"
    setPackageName(env = .Object)
    .Object 
  }
)

setMethod(
  f = "setFileCache",
  signature = signature("CachingEnhancedEnvironment", "FileCache"),
  definition = function(owner, fileCache){
    owner@fileCache <- fileCache
    owner
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
    owner <- tryCatch({
      .cacheObject(owner, name)
    },
      error = function(e){
        oldClass <- class(owner)
        class(owner) <- "EnhancedEnvironment"
        owner <- deleteObject(owner, name)
        class(owner) <- oldClass
        stop(e)
      }
    )
  }
)

setMethod(
  f = "addObject",
  signature = signature("CachingEnhancedEnvironment", "ANY", "missing", "missing"),
  definition = function(owner, object){
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    addObject(owner, object, name)
  }
)

setMethod(
    f = "addObject",
    signature = signature("CachingEnhancedEnvironment", "list", "missing", "logical"),
    definition = function(owner, object, unlist){
      if(!unlist){
        name = deparse(substitute(object, env=parent.frame()))
        name <- gsub("\\\"", "", name)
        owner <- addObject(owner, object, name)
      }else{
        if(any(names(object) ==""))
          stop("All list elements must be named when unlisting")
        
        lapply(names(object), function(n){
              owner[[n]] <- object[[n]]
            })
      }
      invisible(owner)
    }
)


##
## Allows caller to add a single object to the environment using the double bracket
## accessor
##
setReplaceMethod("[[", 
  signature = signature(
    x = "CachingEnhancedEnvironment",
    i = "character"
  )
  ,
  function(x, i, value) {
    addObject(x, value, i)
  }
)


##
## Override the deleteObject method inherited from EnhancedEnvironment. The method
## for CachingEnhancedEnvironment does the same thing as the one for EnhancedEnvironment
## but it also deletes the cached binary from disk. Failure to delete the cache file
## will print a warning but will not prevent the object from being delted from the
## environment.
##
setMethod(
  f = "deleteObject",
  signature = signature("CachingEnhancedEnvironment", "character"),
  definition = function(owner, which){
    owner <- tryCatch(
      .deleteCacheFile(owner, which),
      error = function(e){
        warning(sprintf("Unable to remove cached binary for '%s' object: %s", which, as.character(e)))
        owner
      }
    )
    oldClass <- class(owner)
    class(owner) <- "EnhancedEnvironment"
    owner <- deleteObject(owner, which)
    class(owner) <- oldClass
    invisible(owner)
  }
)

##
## Must also override renameObject method
##
setMethod(
  f = "renameObject",
  signature = signature("CachingEnhancedEnvironment", "character", "character"),
  definition = function(owner, which, name){
    if(length(which) != length(name))
      stop("Must supply the same number of names as objects")
    
    if(!all(which %in% names(owner)))
      stop("Invalid objects provided")
    
    ## temporarily store all objects into an environment. also store the destination objects 
    ## so that things can be put back if an exception is encountered
    tmpEnvSrc <- new.env()
    lapply(which, function(w) assign(w, getObject(owner, w), envir=tmpEnvSrc))
    
    tmpEnvDest <- new.env()
    lapply(intersect(names(owner), name), function(w){
        assign(w, getObject(owner, w), envir=tmpEnvDest)
      })
    
    ## now perform the move by first deleting the objects then adding them back from the
    ## temporary environment, but with their new names. put everything back if an error occurs
    ## or a warning is encountered
    
    ## elevate warnings to errors
    oldWarn <- options("warn")[[1]]
    
    owner <- tryCatch({
        owner <- deleteObject(owner, which)
        lapply(1:length(which), function(i){
          owner <<- addObject(owner, get(which[i], envir=tmpEnvSrc), name[i])
        })
        owner
      },
      error = function(e){
        ## put everything back the way it was an then throw
        ## an exception. it's important to put things back 
        ## manually since the EnhancedEnvironment is pass-by-reference
        
        ## delete all the destination objects
        lapply(intersect(name, names(owner)), function(w) owner <<- deleteObject(owner, w))
        
        ## put the originals back
        lapply(name, function(w){
            owner <<- addObject(owner, get(w, envir=tmpEnvDest), name)
          })
        
        ## now that everything is back to it's starting state, throw an exception
        stop(e)
      },
      finally = options(warn=oldWarn)
    )
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
    files <- object@fileCache$files()
    prefix <- gsub("\\.", "\\\\.",object@cachePrefix)
    suffix <- gsub("\\.", "\\\\.",object@cacheSuffix)
    pattern <- sprintf("^%s.+\\.%s$", prefix, suffix)
    indx <- grep(pattern, files)
    if(length(indx) == 0L)
      return(character())
    files <- files[indx]
    files
  }
)

##
## print the cache directory
##
setMethod(
  f = "cacheDir",
  signature = "CachingEnhancedEnvironment",
  definition = function(object){
    object@fileCache$getCacheDir()
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
    
    ## check that cachesubdir exists
    cacheDir <- owner@fileCache$getCacheDir()
    aDir <- file.path(cacheDir, owner@cachePrefix)
    aDir <- gsub("[\\/]+$", "", aDir)
    if(grepl("/$", owner@cachePrefix) && !file.exists(aDir)){
      cacheDir <- file.path(owner@fileCache$getCacheDir(), owner@cachePrefix)
      dir.create(cacheDir, recursive=TRUE)
    }
    
    ## make sure the cacheDir is a directory
    info <- file.info(cacheDir)
    if(is.na(info$isdir) || !info$isdir)
      stop("could not create directory for holding cached objects: %s", cacheDir)
    
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
    relPath <- gsub(owner@fileCache$getCacheDir(), "", destFile, fixed = TRUE)
    relPath <- gsub("^/+", "", relPath)
    addFileMetaData(owner@fileCache, destFile, relPath)
    
    ## cache the metadata to disk
    owner@fileCache$cacheFileMetaData()
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
  signature = signature("CachingEnhancedEnvironment", "character", "ANY"),
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
  signature = signature("CachingEnhancedEnvironment", "character", "ANY"),
  definition = function(owner, objectName, suffix){
    if(missing(suffix))
      suffix <- owner@cacheTmpSuffix
    if(!is.character(suffix))
      stop("suffix must be a character")
    
    .generateCacheFileRelativePath(owner, objectName, suffix)
  }
)

##
## generate the temporaty cache file name for an object
##
setMethod(
  f = ".generateTmpCacheFileName",
  signature = signature("CachingEnhancedEnvironment", "character"),
  definition = function(owner, objectName)
  {
    .generateCacheFileRelativePath(owner, objectName, owner@cacheTmpSuffix)
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
    object@fileCache <- moveFile(object@fileCache, 
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
    ## if the destination file exists, delete it
    if(.generateCacheFileRelativePath(object, destName) %in% files(object))
      object@fileCache <- deleteFile(object@fileCache, .generateCacheFileRelativePath(object, destName))
    
    object@fileCache <- moveFile(object@fileCache, 
      .generateTmpCacheFileRelativePath(object, srcName), 
      .generateCacheFileRelativePath(object, destName)
    )
    invisible(object)
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
  signature = signature("CachingEnhancedEnvironment", "logical"),
  definition = function(owner, clearEnvironment){
    
    ## first clear out the owner's environment from all existing
    ## objects
    if (clearEnvironment) rm(list= names(owner), envir = as.environment(owner))
    
    ## get the cached files
    ##files <- file.path(owner@fileCache$getCacheDir(), files(owner))
    ##
    ##major hack need to fix this once the FileCache is fixed
    files <- dir(file.path(owner@fileCache$cacheDir, owner@cachePrefix), full.names=T)
    if(length(files == 0L))
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
