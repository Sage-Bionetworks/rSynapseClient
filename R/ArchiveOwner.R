# TODO: Add comment
# 
# Author: furia
###############################################################################

setMethod(
  f = "initialize",
  signature = "ArchiveOwner",
  definition = function(.Object){
    .Object@fileCache <- new("FileCache")
    .Object@objects <- new("EnhancedEnvironment")
    .Object
  }
)

setMethod(
  f = "addFile",
  signature = signature("ArchiveOwner", "character", "missing"),
  definition = function(entity,file, path){
    entity@fileCache <- addFile(entity@fileCache, file)
    invisible(entity)
  }
)

setMethod(
    f = "addFile",
    signature = signature("ArchiveOwner", "character", "character"),
    definition = function(entity,file, path){
      entity@fileCache <- addFile(entity@fileCache, file, path)
      invisible(entity)
    }
)

setMethod(
    f = "deleteFile",
    signature = signature("ArchiveOwner", "character"),
    definition = function(entity,file){
      entity@fileCache <- deleteFile(entity@fileCache, file)
      invisible(entity)
    }
)

setMethod(
    f = "moveFile",
    signature = signature("ArchiveOwner", "character", "character"),
    definition = function(entity,src, dest){
      entity@fileCache <- moveFile(entity@fileCache, src, dest)
      invisible(entity)
    }
)

setMethod(
  f = "cacheDir",
  signature = "ArchiveOwner",
  definition = function(object){
    object@fileCache$getCacheDir()
  }
)

setMethod(
  f = "files",
  signature = "ArchiveOwner",
  definition = function(object){
    object@fileCache$files()
  }
)

setMethod(
  f = "loadObjectsFromFiles",
  signature = "ArchiveOwner",
  definition = function(owner){
    ## default method only loads rbin files
    files <- files(owner)
    indx <- grep("\\.rbin$", tolower(files))
    lapply(indx, function(ii){
          load(file.path(cacheDir(owner), files[ii]), envir=as.environment(owner@objects))
        })
    invisible(owner)
  }
)

setMethod(
  f = "unpackArchive",
  signature = "ArchiveOwner",
  definition = function(owner){
    owner@fileCache$unpackArchive()
  }
) 

setMethod(
  f = "createArchive",
  signature = "ArchiveOwner",
  definition = function(owner){
    owner$fileCache$createArchive()
  }
)

setMethod(
  f = "getArchiveFilePath",
  signature = "ArchiveOwner",
  definition = function(owner){
    owner
  }
)

#objects.ArchiveOwner <-
#    function(name)
#{
#  names(name@objects)
#}


