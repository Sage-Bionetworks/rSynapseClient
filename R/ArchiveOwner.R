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
  f = "files",
  signature = "ArchiveOwner",
  definition = function(object){
    files <- object@fileCache$files()
    indx <- grep(sprintf("^[\\.]?[/\\]?[\\.]?%s", synapseObjectCache()), files)
    if(length(indx) > 0L)
      files <- files[-indx]
    files
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
    file.path(owner@fileCache$getCacheRoot(), owner@fileCache$getArchiveFile())
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

setMethod(
  f = "ArchiveOwner",
  signature = "character",
  definition = function(path, archiveFileName){
    own <- new("ArchiveOwner")
    own@fileCache <- getFileCache(path)
    if(!missing(archiveFileName)){
      own@fileCache$setArchiveFileName(archiveFileName)
    }
    own
  }
)

setMethod(
  f = "ArchiveOwner",
  signature = "missing",
  definition = function(){
    own <- new("ArchiveOwner")
    own@fileCache <- getFileCache(tempfile())
    own
  }
)

getPackageName.ArchiveOwner <-
  function (where, create = TRUE)
{
  getPackageName(where = where@objects, create = create)
}






