# TODO: Add comment
# 
# Author: furia
###############################################################################

####
# Methods for adding general objects
####
setMethod(
  f = "addObject",
  signature = signature("ObjectOwner", "ANY", "character", "missing"),
  definition = function(owner, object, name){
    owner$objects[[name]] <- object
    invisible(owner)
  }
)

setMethod(
  f = "addObject",
  signature = signature("ObjectOwner", "ANY", "missing", "missing"),
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
  signature = signature("ObjectOwner", "list", "character", "logical"),
  definition = function(owner, object, name, unlist){
    if(unlist)
      stop("cannot specify the object name when unlisting")
    addObject(owner, object, name)
  }
)


setMethod(
  f = "addObject",
  signature = signature("ObjectOwner", "list", "missing", "logical"),
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
  signature = signature("ObjectOwner", "list", "missing", "missing"),
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
  signature = signature("ObjectOwner", "data.frame", "missing", "missing"),
  definition = function(owner, object){
    name = deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    addObject(owner, object, name, unlist = FALSE)
  }
)

setMethod(
  f = "addObject",
  signature = signature("ObjectOwner", "data.frame", "character", "missing"),
  definition = function(owner, object, name){
    addObject(owner, object, name)
  }
)

setMethod(
  f = "deleteObject",
  signature = signature("ObjectOwner", "character"),
  definition = function(owner, which){
    deleteObject(getEnv(owner), which)
    invisible(owner)
  }
)

setMethod(
  f = "renameObject",
  signature = signature("ObjectOwner", "character", "character"),
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

setMethod(
  f = "getObject",
  signature = signature("ObjectOwner", "character"),
  definition = function(owner, which){
    nms <- names(owner)
    indx <- which(nms %in% which)
    if(length(indx) == 0)
      return(NULL)
    objs <- lapply(nms[indx], function(n) get(n, envir=owner$getEnv()))
    names(objs) <- nms[indx]
    objs
  }
)


setMethod(
  f = "getEnv",
  signature = "ObjectOwner",
  definition = function(object){
    object$getEnv()
  }
)

## return the environment wrapped by the enclosed EnhancedEnvironment class
## should this be done? May not need this method since it encourages callers
## not to use the API. Commenting it out for now and will add it back later if
## it's needed
#setMethod(
#  f = "getEnv",
#  signature = "ObjectOwner",
#  definition = function(object){
#    object$getEnv()
#  }
#)

## delegate "objects" calls to the enclosed EnhancedObjects class
objects.ObjectOwner <-
  function(name, all.names, pattern)
{
  objects (name@objects, all.names, pattern) 
}

## delegate "names" calls to the enclosed EnhancedObjects class
names.ObjectOwner <-
  function(x)
{
  c("cacheDir", "files", "objects")
  ##names(x$objects) 
}





