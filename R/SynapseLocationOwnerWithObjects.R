# TODO: Add comment
# 
# Author: mfuria
###############################################################################

setMethod(
  f = "SynapseLocationOwnerWithObjects",
  signature = "list",
  definition = function(entity){
    ee <- new("SynapseLocationOwnerWithObjects")
    ee@properties <- entity
    ee
  }
)

#setMethod(
#  f = "downloadEntity",
#  signature = "SynapseLocationOwnerWithObjects",
#  definition = function(entity){
#    ## check whether user has signed agreement
#    ## euals are broken. ignore for now
##    if(!hasSignedEula(entity)){
##      if(!.promptSignEula())
##        stop(sprintf("Visit https://synapse.sagebase.org to sign the EULA for entity %s", propertyValue(entity, "id")))
##      if(!.promptEulaAgreement(entity))
##        stop("You must sign the EULA to download this dataset. Visit http://synapse.sagebase.org for more information.")
##      .signEula(entity)
##    }
#    
#    ## download the archive from S3
#    ## Note that we just use the first location, to future-proof this we would use the location preferred
#    ## by the user, but we're gonna redo this in java so no point in implementing that here right now
#    dfun <- getMethod("downloadEntity", "SynapseLocationOwner")
#    ee <- dfun(entity)
#    ee@objOwn <- entity@objOwn
#    
#    entity
#  }
#)



setMethod(
  f = "getEntity",
  signature = "SynapseLocationOwnerWithObjects",
  definition = function(entity){
    gfun <- getMethod("getEntity", "SynapseLocationOwner")
    ee <- gfun(entity)
    ee@objOwn <- entity@objOwn
    ee
  }
)
setMethod(
  f = "storeEntity",
  signature = "SynapseLocationOwnerWithObjects",
  definition = function(entity){
    if((length(data$files) + length(data$objects)) > 0L){
      ## create the archive on disk (which will persist file metaData to disk)
      createArchive(entity@archOwn)
      
      ## upload the archive  file (storeFile also updates the entity)
      entity <- storeFile(entity ,file.path(entity@archOwn@fileCache$getCacheRoot(), entity@archOwn@fileCache$getArchiveFile()))

    }else{
      if(is.null(propertyValue(entity, "id")))
      {
        entity <- createEntity(entity)
      }else{
        entity <- updateEntity(entity)
      }
    }
    entity
  }
)

setMethod(
  f = "storeEntityObjects",
  signature = "SynapseLocationOwnerWithObjects",
  definition = function(entity){
    storeEntity(entity)
  }
)



setMethod(
    f = "updateEntity",
    signature = "SynapseLocationOwnerWithObjects",
    definition = function(entity){
      ufun <- getMethod("updateEntity", "SynapseLocationOwner")
      updatedEntity <- ufun(entity)
      updatedEntity@objOwn <- entity@objOwn
      updatedEntity
    }
)
setMethod(
  f = "createEntity",
  signature = "SynapseLocationOwnerWithObjects",
  definition = function(entity){
    cfun <- getMethod("createEntity", "SynapseLocationOwner")
    ee <- cfun(entity)
    ee@objOwn <- entity@objOwn
    ee
  }
)

setMethod(
  f = "downloadEntity",
  signature = "SynapseLocationOwnerWithObjects",
  definition = function(entity){
    ## call the superclass method
    dlfun <- getMethod("downloadEntity", signature="SynapseLocationOwner")
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
    signature = signature("SynapseLocationOwnerWithObjects", "ANY", "character", "missing"),
    definition = function(owner, object, name){
      owner@objOwn <- addObject(owner@objOwn, object, name)
      invisible(owner)
    }
)

setMethod(
    f = "addObject",
    signature = signature("SynapseLocationOwnerWithObjects", "ANY", "missing", "missing"),
    definition = function(owner, object){
      name = deparse(substitute(object, env=parent.frame()))
      name <- gsub("\\\"", "", name)
      addObject(owner, object, name)
    }
)

setMethod(
    f = "addObject",
    signature = signature("SynapseLocationOwnerWithObjects", "list", "missing", "logical"),
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
    signature = signature("SynapseLocationOwnerWithObjects", "character"),
    definition = function(owner, which){
      owner@objOwn <- deleteObject(owner@objOwn, which)
      invisible(owner)
    }
)

setMethod(
    f = "renameObject",
    signature = signature("SynapseLocationOwnerWithObjects", "character", "character"),
    definition = function(owner, which, name){
      owner@objOwn <- renameObject(owner@objOwn, which, name)
      invisible(owner)
    }
)

setMethod(
    f = "getObject",
    signature = signature("SynapseLocationOwnerWithObjects", "character"),
    definition = function(owner, which){
      getObject(owner@objOwn, which)
    }
)

setMethod(
    f = "files",
    signature = "SynapseLocationOwnerWithObjects",
    definition = function(object){
      files(object@objOwn)
    }
)

setMethod(
    f = "cacheDir",
    signature = "SynapseLocationOwnerWithObjects",
    definition = function(object){
      cacheDir(object@objOwn)
    }
)

objects.SynapseLocationOwnerWithObjects <-
    function(x)
{
  objects(x@objOwn)
}

setMethod(
    f = "[",
    signature = "SynapseLocationOwnerWithObjects",
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
                files = files(x@archOwn),
                fileObjects = x@archOwn@objects,
                binObjects = x@objOwn$objects[],
                NULL
            )
          }
      )
      names(retVal) <- i
      retVal
    }
)

setMethod(
  f = ".doGetObjects",
  signature = "SynapseLocationOwnerWithObjects",
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
  signature = "SynapseLocationOwnerWithObjects",
  definition = function(.Object){
    .Object@archOwn <- new("ArchiveOwner")
    .Object@objOwn <- new("CachingObjectOwner")
    .Object
  }
)

names.SynapseLocationOwnerWithObjects <-
    function(x)
{
  c("objects", "cacheDir","files", "fileObjects", "binObjects")
}

setMethod(
  f = "summarizeObjects",
  signature = "SynapseLocationOwnerWithObjects",
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

objects.SynapseLocationOwnerWithObjects <-
  function(name)
{
  union(objects(own@archOwn@objects, all.names=T), objects(own@objOwn$objects, all.names=T))
}


setMethod(
  f = "loadObjectsFromFiles",
  signature = "SynapseLocationOwnerWithObjects",
  definition = function(owner){
    ## call the superclass method
    lfun <- getMethod("loadObjectsFromFiles", "SynapseLocationOwner")
    owner <- lfun(owner)
    owner@objOwn <- loadObjectsFromFiles(owner@objOwn)
    invisible(owner)
  }
)



