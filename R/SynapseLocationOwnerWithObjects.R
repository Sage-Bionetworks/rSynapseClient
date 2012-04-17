# TODO: Add comment
# 
# Author: mfuria
###############################################################################


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
        any(names(object) == "")
        stop("All list elements must be named when unlisting")
        owner@objOwn <- addObject(owner, object, name, unlist)
      }else{
        owner <- addObject(owner, object)
      }
      invisible(object)
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
                objects = x@archOwn@objects,
                cacheDir = cacheDir(x@archOwn),
                files = files(x@archOwn),
                fileObjects = objects(x@archOwn),
                binObjects = objects(x@objOwn),
                NULL
            )
          }
      )
      names(retVal) <- i
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






