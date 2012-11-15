# TODO: Add comment
#
# Author: mfuria
###############################################################################

setMethod(
  f = "Locationable",
  signature = "list",
  definition = function(entity){
    ee <- new("Locationable")
    ee@properties <- entity
    ee
  }
)

setMethod(
  f = "attach",
  signature = signature(what = "Locationable"),
  definition = function (what, pos = 2, name = getPackageName(what@objOwn), warn.conflicts = TRUE){
    attach(what@objOwn, pos = pos, name = name, warn.conflicts = warn.conflicts)

    afun <- getMethod('attach', 'SynapseLocationOwner')
    afun(what, pos = pos, warn.conflicts = warn.conflicts)
  }
)

setMethod(
  f = "detach",
  signature = signature(name = "Locationable"),
  definition = function (name)
  {
    detach(name@objOwn)
    detach(name@archOwn)
  }
)

setMethod(
	f = "loadEntity",
	signature = signature("Locationable","missing"),
	definition = function(entity){

    lfun <- getMethod("loadEntity", signature("SynapseLocationOwner", "missing"))
    entity <- lfun(entity)
    entity@objOwn$objects@fileCache <- entity@archOwn@fileCache
		entity@objOwn <- loadObjectsFromFiles(entity@objOwn)

		entity
	}
)

setMethod(
  f = "loadEntity",
  signature = signature("Locationable","character"),
  definition = function(entity, versionId){

    lfun <- getMethod("loadEntity", signature("SynapseLocationOwner", "character"))
    entity <- lfun(entity, versionId)
    entity@objOwn$objects@fileCache <- entity@archOwn@fileCache
    entity@objOwn <- loadObjectsFromFiles(entity@objOwn)

    entity
  }
)

setMethod(
  f = "loadEntity",
  signature = signature("Locationable","numeric"),
  definition = function(entity, versionId){
    loadEntity(entity, as.character(versionId))
  }
)

setMethod(
  f = "getEntity",
  signature = signature("Locationable", "missing"),
  definition = function(entity){
    gfun <- getMethod("getEntity", signature("SynapseLocationOwner", "missing"))
    ee <- gfun(entity)
    ee@objOwn <- entity@objOwn
    ee
  }
)

setMethod(
  f = "storeEntityObjects",
  signature = "Locationable",
  definition = function(entity){
    storeEntity(entity)
  }
)

setMethod(
  f = "updateEntity",
  signature = "Locationable",
  definition = function(entity){
    ufun <- getMethod("updateEntity", "SynapseLocationOwner")
    updatedEntity <- ufun(entity)
    updatedEntity@objOwn <- entity@objOwn
    updatedEntity
  }
)
setMethod(
  f = "createEntity",
  signature = "Locationable",
  definition = function(entity){
    cfun <- getMethod("createEntity", "SynapseLocationOwner")
    ee <- cfun(entity)
    ee@objOwn <- entity@objOwn
    ee
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("Locationable", "missing"),
  definition = function(entity){
    ## call the superclass method
    dlfun <- getMethod("downloadEntity", signature=signature("SynapseLocationOwner", "missing"))
    ee <- dlfun(entity)
    ee@objOwn <- entity@objOwn

    ## now set the archive for the caching object owner to be the same
    ## as the one for the archive owner
    oo <- entity@objOwn
    setFileCache(oo, entity@archOwn@fileCache)
    ee@objOwn <- oo

    ee
  }
)


setMethod(
  f = "addObject",
  signature = signature("Locationable", "ANY", "character", "missing"),
  definition = function(owner, object, name){
    owner@objOwn <- addObject(owner@objOwn, object, name)
    invisible(owner)
  }
)

setMethod(
  f = "addObject",
  signature = signature("Locationable", "ANY", "missing", "missing"),
  definition = function(owner, object){
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    addObject(owner, object, name)
  }
)

setMethod(
  f = "addObject",
  signature = signature("Locationable", "list", "missing", "logical"),
  definition = function(owner, object, unlist){
    if(unlist){
      if(any(names(object) == ""))
        stop("All list elements must be named when unlisting")
      lapply(names(object), function(n){
          owner <<- addObject(owner, object[[n]], n)
        })
    }else{
      owner <- addObject(owner, object)
    }
    invisible(owner)
  }
)

setMethod(
  f = "deleteObject",
  signature = signature("Locationable", "character"),
  definition = function(owner, which){
    owner@objOwn <- deleteObject(owner@objOwn, which)
    invisible(owner)
  }
)

setMethod(
  f = "renameObject",
  signature = signature("Locationable", "character", "character"),
  definition = function(owner, which, name){
    owner@objOwn <- renameObject(owner@objOwn, which, name)
    invisible(owner)
  }
)

setMethod(
  f = "getObject",
  signature = signature("Locationable", "character"),
  definition = function(owner, which){
    getObject(owner@objOwn, which)
  }
)

setMethod(
  f = "files",
  signature = "Locationable",
  definition = function(object){
    setdiff(files(object@archOwn), files(object@objOwn))
  }
)

setMethod(
  f = "cacheDir",
  signature = "Locationable",
  definition = function(object){
    cacheDir(object@objOwn)
  }
)

objects.Locationable <-
  function(x)
{
  objects(x@objOwn)
}

setMethod(
  f = "[",
  signature = "Locationable",
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
        switch(i,
          objects = .doGetObjects(x),
          cacheDir = cacheDir(x@archOwn),
          files = files(x),
          fileObjects = x@archOwn@objects,
          binObjects = x@objOwn$objects[],
          if(i %in% names.Entity(x)){
              class(x) <- "Entity"
              return(x[[i]])
            }else{
              return(NULL)
            }
        )
      }
    )
    names(retVal) <- i
    retVal
  }
)

setMethod(
  f = ".doGetObjects",
  signature = "Locationable",
  definition = function(x){
    oo <- union(objects(x@archOwn@objects, all.names=T), objects(x@objOwn$objects, all.names=T))
    retVal <- lapply(oo, function(oName){
        if(oName %in% objects(x@archOwn@objects, all.names=T))
          return(x@archOwn@objects[[oName]])
        return(x@objOwn$objects[[oName]])
      })
    names(retVal) <- oo
    retVal
  }
)

setMethod(
  f = "initialize",
  signature = "Locationable",
  definition = function(.Object){
    .Object@archOwn <- new("ArchiveOwner")
    .Object@archOwn@fileCache <- getFileCache(.Object@archOwn@fileCache$getCacheRoot())
    .Object@objOwn <- new("CachingObjectOwner")
    .Object@objOwn$objects@fileCache <- .Object@archOwn@fileCache
    .Object
  }
)

names.Locationable <-
  function(x)
{
  c("objects", "cacheDir","files", "fileObjects", "binObjects", names.Entity(x))
}

setMethod(
  f = "summarizeObjects",
  signature = "Locationable",
  definition = function(entity){
    msg <- NULL
    if(length(names(entity$objects)) > 0){
      msg$count <- sprintf("loaded object(s)")
      objects <- names(entity$objects)
      classes <- unlist(lapply(objects, function(object){paste(class(entity$objects[[object]]), collapse=":")}))
      msg$objects <- sprintf('[%d] "%s" (%s)', 1:length(objects), objects, classes)
    }
    msg
  }
)

objects.Locationable <-
  function(name)
{
  union(objects(name@archOwn@objects, all.names=T), objects(name@objOwn$objects, all.names=T))
}

setMethod(
  f = "loadObjectsFromFiles",
  signature = "Locationable",
  definition = function(owner){
    ## call the superclass method
    lfun <- getMethod("loadObjectsFromFiles", signature("SynapseLocationOwner"))
    owner <- lfun(owner)
    owner@objOwn <- loadObjectsFromFiles(owner@objOwn)
    invisible(owner)
  }
)

setReplaceMethod("$",
  signature = "SynapseLocationOwner",
  definition = function(x, name, value) {
     if(name == "objects"){
       stop("not yet supported, use the addObject method for adding objects", call.=FALSE)
    }else if(name %in% names.SynapseLocationOwner(x)){
      slot(x, name) <- value
    }else{
      stop("invalid element")
    }
    x
  }
)
