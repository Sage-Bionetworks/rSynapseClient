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
    setPackageName(env = .Object)
    .Object
  }
)

setMethod(
  f = "getEntity",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    gfun <- getMethod("getEntity", "SynapseEntity")
    ee <- gfun(entity)
    ee@archOwn <- entity@archOwn
    ee
  }
)


setMethod(
  f = "setCacheRoot",
  signature = signature("ArchiveOwner", "character", "logical", "missing"),
  definition = function(object, path, clean){
    object@fileCache <- setCacheRoot(object@fileCache, path, clean)
    object
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
    owner
  }
) 

setMethod(
  f = "createArchive",
  signature = "ArchiveOwner",
  definition = function(owner){
    owner@fileCache$createArchive()
  }
)

setMethod(
  f = "getArchiveFilePath",
  signature = "ArchiveOwner",
  definition = function(owner){
    owner
  }
)

setMethod(
  f = "setPackageName",
  signature = signature(env = "ArchiveOwner"),
  definition = function(pkg, env)
  {
    if(missing(pkg))
      pkg <- basename(tempfile(pattern=as.character(class(env))))
    setPackageName(pkg = pkg, env = env@objects)
  }
)

as.environment.ArchiveOwner <-
  function(x)
{
  as.environment(x@objects)
}

setMethod(
    f = "attach",
    signature = signature(what = "ArchiveOwner"),
    definition = function (what, pos = 2, name = getPackageName(what), warn.conflicts = TRUE)
    {
      attach(what@objects, pos = pos, name = name, warn.conflicts = warn.conflicts)
    }
)

setMethod(
    f = "detach",
    signature = signature(name = "ArchiveOwner"),
    definition = function (name)
    {
      detach(name@objects)
    }
)

getPackageName.ArchiveOwner <-
  function (where, create = TRUE)
{
  getPackageName(where = where@objects, create = create)
}


