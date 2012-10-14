# TODO: Add comment
#
# Author: mfuria
###############################################################################

setMethod(
  f = "initialize",
  signature = "SynapseLocationOwner",
  definition = function(.Object){
    .Object@archOwn <- new("ArchiveOwner")
    .Object@archOwn@fileCache <- getFileCache(.Object@archOwn@fileCache$getCacheRoot())
    setPackageName(env=.Object)
    .Object
  }
)


setMethod(
    f = "loadEntity",
    signature = signature("SynapseLocationOwner", "missing"),
    definition = function(entity){
      if(!is.null(entity$properties$id))
        entity <- downloadEntity(entity)
      entity@archOwn <- loadObjectsFromFiles(entity@archOwn)
      setFetchMethod(entity, "load")
      entity
    }
)

setMethod(
    f = "loadEntity",
    signature = signature("SynapseLocationOwner", "character"),
    definition = function(entity, versionId){
      ##if(length(entity$files) == 0)
      entity <- downloadEntity(entity, versionId)
      entity@archOwn <- loadObjectsFromFiles(entity@archOwn)
      setFetchMethod(entity, "load")
      entity
    }
)

setMethod(
  f = "loadEntity",
  signature = signature("SynapseLocationOwner", "numeric"),
  definition = function(entity, versionId){
    loadEntity(entity, as.character(versionId))
  }
)


setMethod(
  f = "storeEntity",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    if(is.null(propertyValue(entity, "id"))){
      entity <- createEntity(entity)
    }else{
      method <- synapseClient:::getFetchMethod(entity)
      entity <- updateEntity(entity)
      if(!is.null(method))
        synapseClient:::setFetchMethod(entity, method)
    }

    if(!is.null(getFetchMethod(entity)) && getFetchMethod(entity) == "load"){
      ## create the archive on disk (which will persist file metaData to disk)
      file <- createArchive(entity@archOwn)

      if(!is.null(file)){
        ## upload the archive  file (storeFile also updates the entity)
        file <- file.path(entity@archOwn@fileCache$getCacheRoot(), file)
        entity <- storeFile(entity, file)
      }else{
        if(!is.null(entity$properties$locations)){
          entity <- deleteProperty(entity, "locations")
          entity <- updateEntity(entity)
        }
      }
    }
    entity
  }
)

setMethod(
  f = "storeEntityFiles",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    storeEntity(entity)
  }
)


setMethod(
  f = "createEntity",
  signature = "SynapseLocationOwner",
  definition = function(entity){ 
    entity@archOwn@fileCache <- getFileCache(getFileCacheName(entity@archOwn@fileCache))
    archOwn <- entity@archOwn
    cfun <- getMethod("createEntity", "SynapseEntity")
    entity <- cfun(entity)
    entity@archOwn <- setCacheRoot(archOwn, entity@archOwn@fileCache$getCacheRoot(), TRUE )
    entity
  }
)


setMethod(
  f = "storeFile",
  signature = signature("SynapseLocationOwner", "character"),
  definition = function(entity, filePath) {

    if(!file.exists(filePath))
      stop('archive file does not exist')

    ## make sure that the entity is up to date
    if(is.null(propertyValue(entity, "id"))){
      ## Create the LocationOwner in Synapse
      entity <- createEntity(entity)
      filePath <- file.path(entity@archOwn@fileCache$getCacheRoot(), basename(filePath))
    }

    entity <- tryCatch(
      if(synapseClient:::.getCache("useJava")){
        synapseClient:::.performMultipartUpload(entity, filePath)
      }else{
        .performRUpload(entity, filePath)
      },
      error = function(e){
        warning(sprintf("failed to upload data file, please try again: %s", e))
        return(entity)
      }
    )

    ## move the data file from where it is to the local cache directory
    url <- entity$properties$locations[[1]][['path']]
    if(is.null(url))
      stop("NULL URL")
    destdir <- .generateCacheDestDir(url, entity$properties$versionNumber)

    ## set the cachRoot to the new location this method should do the right
    ## thing. don't move if src and dest are the same. make sure there are
    ## no straggler files left behind, clean up temp directories, etc.
    entity@archOwn <- setCacheRoot(entity@archOwn, destdir, clean = TRUE)

    ## make sure the fileCache gets added to the FileCacheFactory
    ##entity@archOwn <- ArchiveOwner(destdir)
    if(inherits(entity, "SynapseLocationOwnerWithObjects"))
      setFileCache(entity@objOwn, entity@archOwn@fileCache)

    ## unpack the archive into it's new root directory.
    entity@archOwn <- unpackArchive(entity@archOwn)
    invisible(entity)
  }
)

setMethod(
  f = "getEntity",
  signature = signature("SynapseLocationOwner", "missing"),
  definition = function(entity){
    gfun <- getMethod("getEntity", signature("SynapseEntity", "missing"))
    ee <- gfun(entity)
    ee@archOwn <- entity@archOwn
    ee
  }
)

setMethod(
    f = "updateEntity",
    signature = "SynapseLocationOwner",
    definition = function(entity){
      ufun <- getMethod("updateEntity", "SynapseEntity")
      updatedEntity <- ufun(entity)
      slot(updatedEntity, "archOwn") <- entity@archOwn
      updatedEntity
    }
)

setMethod(
  f = "downloadEntity",
  signature = signature("SynapseLocationOwner", "missing"),
  definition = function(entity){
    downloadEntity(entity, entity$properties$versionNumber)
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("SynapseLocationOwner", "numeric"),
  definition = function(entity, versionId){
    downloadEntity(entity, as.character(versionId))
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("SynapseLocationOwner", "character"),
  definition = function(entity, versionId){
    if(versionId != as.character(entity$properties$versionNumber))
      entity <- getEntity(entity, versionId)

    if(is.null(propertyValue(entity, "locations")[[1]][['path']]))
      return(entity)

    ## download the file
    url <- entity$properties$locations[[1]][['path']]
    destfile <- .generateCacheDestFile(url, entity$properties$versionNumber)
    if(entity@archOwn@fileCache$archiveFile != basename(destfile)){
      entity@archOwn <- ArchiveOwner(dirname(destfile), basename(destfile))
    } else {
      entity@archOwn <- ArchiveOwner(dirname(destfile))
    }


    ## passing the md5 sum causes this funciton to only download the file
    ## if the cached copy does not match that md5 sum
    archiveFile <- synapseClient:::synapseDownloadFile(url, propertyValue(entity, "md5"), versionId=entity$properties$versionNumber)

    archiveFile <- normalizePath(archiveFile)
    if(entity@archOwn@fileCache$archiveFile != basename(destfile)){
      entity@archOwn <- ArchiveOwner(dirname(destfile), basename(destfile))
    } else {
      entity@archOwn <- ArchiveOwner(dirname(destfile))
    }
    ## unpack the archive
    entity@archOwn <- unpackArchive(entity@archOwn)

    entity
  }
)

setMethod(
    f = "deleteEntity",
    signature = "SynapseLocationOwner",
    definition = function(entity){
      entity@archOwn@fileCache$delete()
      dfun <- getMethod("deleteEntity", "SynapseEntity")
      entity <- dfun(entity)
      invisible(entity)
    }
)


setMethod(
  f = "addFile",
  signature = signature("SynapseLocationOwner", "character", "character"),
  definition = function(entity, file, path){
    entity@archOwn <- addFile(entity@archOwn, file, path)
    invisible(entity)
  }
)

setMethod(
    f = "addFile",
    signature = signature("SynapseLocationOwner", "character", "missing"),
    definition = function(entity, file){
      entity@archOwn <- addFile(entity@archOwn, file)
      invisible(entity)
    }
)

setMethod(
    f = "moveFile",
    signature = signature("SynapseLocationOwner", "character", "character"),
    definition = function(entity, src, dest){
      entity@archOwn <- moveFile(entity@archOwn, src, dest)
      invisible(entity)
    }
)

setMethod(
    f = "deleteFile",
    signature = signature("SynapseLocationOwner", "character"),
    definition = function(entity, file){
      entity@archOwn <- deleteFile(entity@archOwn, file)
      invisible(entity)
    }
)

names.SynapseLocationOwner <-
    function(x)
{
  c("objects", "cacheDir", "files", names.SynapseEntity(x))
}


setMethod(
    f = "[",
    signature = "SynapseLocationOwner",
    definition = function(x, i, j, ...){
      if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
        stop("incorrect number of subscripts")
      if(is.numeric(i)){
        if(any(i > length(names(x))))
          stop("subscript out of bounds")
        i <- names(x)[i]
      }else if(is.character(i)){
        if(!all(i %in% names(x)))
          stop("undefined objects selected")
      }else{
        stop(sprintf("invalid subscript type '%s'", class(i)))
      }
      retVal <- lapply(i, function(i){
            if(i=="objects"){
              retVal <- x@archOwn@objects
            }else if(i == "cacheDir"){
              retVal <- cacheDir(x@archOwn)
            }else if(i == "files"){
              retVal <- files(x)
            }else if(i %in% names.SynapseEntity(x)){
              class(x) <- "SynapseEntity"
              x[[i]]
            }else{
              retVal <- NULL
            }
          }
      )
      names(retVal) <- i
      retVal
    }
)

setMethod(
  f = "files",
  signature = "SynapseLocationOwner",
  definition = function(object){
    files(object@archOwn)
  }
)


setMethod(
    f = "[[",
    signature = "SynapseLocationOwner",
    definition = function(x, i, j, ...){
      if(length(as.character(as.list(substitute(list(...)))[-1L])) > 0L || !missing(j))
        stop("incorrect number of subscripts")
      if(length(i) > 1)
        stop("subscript out of bounds")
      x[i][[1]]
    }
)

setMethod(
    f = "$",
    signature = "SynapseLocationOwner",
    definition = function(x, name){
      x[[name]]
    }
)

setMethod(
  f = "show",
  signature = "SynapseLocationOwner",
  definition = function(object){
    cat('An object of class "', class(object), '"\n', sep="")

    cat("Synapse Entity Name : ", properties(object)$name, "\n", sep="")
    cat("Synapse Entity Id   : ", properties(object)$id, "\n", sep="")

    if (!is.null(properties(object)$parentId))
      cat("Parent Id           : ", properties(object)$parentId, "\n", sep="")
    if (!is.null(properties(object)$type))
      cat("Type                : ", properties(object)$type, "\n", sep="")
    if (!is.null(properties(object)$versionNumber)) {
      cat("Version Number      : ", properties(object)$versionNumber, "\n", sep="")
      cat("Version Label       : ", properties(object)$versionLabel, "\n", sep="")
    }

    obj.msg <- summarizeObjects(object)
    if(!is.null(obj.msg)){
      cat("\n", obj.msg$count,":\n", sep="")
      cat(obj.msg$objects, sep="\n")
    }

    files.msg <- summarizeCacheFiles(object)
    if(!is.null(files.msg))
      cat("\n", files.msg$count, "\n", sep="")
    if(!is.null(propertyValue(object,"id"))){
      cat("\nFor complete list of annotations, please use the annotations() function.\n")
      cat(sprintf("To view this Entity on the Synapse website use the 'onWeb()' function\nor paste this url into your browser: %s\n", object@synapseWebUrl))
    }
  }
)

setMethod(
  f = "summarizeObjects",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    msg <- NULL
    if(length(entity$objects  ) > 0){
      msg$count <- sprintf("loaded object(s)")
      objects <- objects(entity$objects)
      classes <- unlist(lapply(objects, function(object){paste(class(entity$objects[[object]]), collapse=":")}))
      msg$objects <- sprintf('[%d] "%s" (%s)', 1:length(objects), objects, classes)
    }
    msg
  }
)

setMethod(
  f = "summarizeCacheFiles",
  signature = "SynapseLocationOwner",
  definition = function(entity){
    ## if Cached Files exist, print them out
    msg <- NULL
    if(length(entity$cacheDir) != 0){
      msg$count <- sprintf('%d File(s) cached in "%s"', length(entity$files), entity$cacheDir)
      if(length(entity$files) > 0)
        msg$files <- sprintf('[%d] "%s"',1:length(entity$files), entity$files)
    }
    msg
  }
)

setMethod(
  f = "loadObjectsFromFiles",
  signature = "SynapseLocationOwner",
  definition = function(owner){
    owner@archOwn <- loadObjectsFromFiles(owner@archOwn)
    invisible(owner)
  }
)

setMethod(
  f = "attach",
  signature = signature(what = "SynapseLocationOwner"),
  definition = function (what, pos = 2, name = getPackageName(what), warn.conflicts = TRUE){
    attach(what@archOwn, pos= pos, name = name, warn.conflicts = warn.conflicts)
  }
)

setMethod(
  f = "detach",
  signature = signature(name = "SynapseLocationOwner"),
  definition = function (name)
  {
    detach(name@archOwn)
  }
)

setMethod(
  f = "setPackageName",
  signature = signature(env = "SynapseLocationOwner"),
  definition = function(pkg, env)
  {
    if(missing(pkg))
      pkg <- basename(tempfile(pattern=as.character(class(env))))
    setPackageName(pkg = pkg, env = env@archOwn)
  }
)

setMethod(
  f = "getPackageName",
  signature = signature(where = "SynapseLocationOwner"),
  definition = function (where, create = TRUE)
  {
    getPackageName(where = where@archOwn, create = create)
  }
)


setReplaceMethod("$",
  signature = "SynapseLocationOwner",
  definition = function(x, name, value) {
     if(!(name %in% names.SynapseEntity(x)))
      stop("invalid element")
    fun <- getMethod("$<-", "SynapseEntity")
    fun(x, name, value)
  }
)

setMethod(
  f = "setFetchMethod",
  signature = signature("SynapseLocationOwner", "character", "FileCacheFactory"),
  definition = function(object, method, factory){
    setFetchMethod(object@archOwn, method, factory)
  }
)


setMethod(
  f = "setFetchMethod",
  signature = signature("SynapseLocationOwner", "character", "missing"),
  definition = function(object, method){
    setFetchMethod(object@archOwn, method)
  }
)

setMethod(
  f = "getFetchMethod",
  signature = signature("SynapseLocationOwner", "FileCacheFactory"),
  definition = function(object, factory){
    getFetchMethod(object@archOwn, factory)
  }
)

setMethod(
  f = "getFetchMethod",
  signature = signature("SynapseLocationOwner", "missing"),
  definition = function(object){
    getFetchMethod(object@archOwn)
  }
)

