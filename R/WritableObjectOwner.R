# TODO: Add comment
# 
# Author: furia
###############################################################################



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

####
# Methods for adding general objects
####
setMethod(
  f = "addObject",
  signature = signature("WritableObjectOwner", "ANY", "character", "missing"),
  definition = function(owner, object, name){
    invisible(.doAddObjectWithCache(owner, object, name))
  }
)

setMethod(
  f = "addObject",
  signature = signature("WritableObjectOwner", "ANY", "missing", "missing"),
  definition = function(owner, object){
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    addObject(owner, object, name)
  }
)

###
# Methods for adding list objects
###
setMethod(
  f = "addObject",
  signature = signature("WritableObjectOwner", "list", "character", "logical"),
  definition = function(owner, object, name, unlist){
    if(unlist)
      stop("cannot specify the object name when unlisting")
    invisible(.doAddObjectWithCache(owner, object, name))
  }
)


setMethod(
  f = "addObject",
  signature = signature("WritableObjectOwner", "list", "missing", "logical"),
  definition = function(owner, object, unlist){
    if(unlist)
      return(addObject(owner, object))
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    addObject(owner, object, name, unlist)
  }
)


setMethod(
  f = "addObject",
  signature = signature("WritableObjectOwner", "list", "missing", "missing"),
  definition = function(owner, object){
    if(any(names(object) == "") || is.null(names(object)))
      stop("all elements of the list must be named")
    lapply(names(object), FUN=function(nm){
        addObject(owner, object[[nm]], nm)
      }
    )
    invisible(owner)
  }
)


####
# Methods for adding data.frame objects
####
setMethod(
  f = "addObject",
  signature = signature("WritableObjectOwner", "data.frame", "missing", "missing"),
  definition = function(owner, object){
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    addObject(owner, object, name, unlist = FALSE)
  }
)

setMethod(
  f = "addObject",
  signature = signature("WritableObjectOwner", "data.frame", "character", "missing"),
  definition = function(owner, object, name){
    invisible(.doAddObjectWithCache(owner, object, name))
  }
)

setMethod(
  f = "deleteObject",
  signature = signature("WritableObjectOwner", "character"),
  definition = function(owner, which){
    rm(list=which, envir=as.environment(owner$objects))
    tryCatch(
      .deleteCacheFile(owner, which),
      error = function(e){
        warning(sprintf("Unable to delete cache file associated with %s\n%s", which, e))
      },
      warning = function(e){
        warning(sprintf("Unable to delete cache file associated with %s\n%s", which, e))
      }
    )
    invisible(owner)
  }
)

setMethod(
  f = "renameObject",
  signature = signature("WritableObjectOwner", "character", "character"),
  definition = function(owner, which, name){
    if(length(which) != length(name))
      stop("Must supply the same number of names as objects")
    
    ## make a copy of the objects that will be moved and delete them from
    ## the entity
    ## TODO : make this more performant by only making copies of objects
    ## when absolutely necessary
    tmpEnv <- new.env()
    lapply(which, FUN = function(key){
        assign(key, getObject(owner, key), envir = tmpEnv)
        deleteObject(owner, key)
      }
    )
    
    lapply(1:length(which), FUN=function(i){
        addObject(owner, get(which[i], envir=tmpEnv), name[i])
      }
    )
    rm(tmpEnv)
    invisible(owner)
  }
)






